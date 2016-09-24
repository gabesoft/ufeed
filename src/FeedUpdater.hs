{-# LANGUAGE TemplateHaskell #-}

-- |
-- Updates a feed
-- Updating a feed consists of fetching the latest feed data,
-- uploading the updated feed and any new posts to database,
-- and indexing the new posts with the search engine
module FeedUpdater
  ( update
  , PostMap
  , UpdateState(..)
  , UpdateEnv(..)
  , envForAddNew
  , envForUpdate
  , latestPosts
  , processFeed
  , runUpdateM
  , updateFeed
  ) where

import qualified Api
import Control.Applicative ((<|>))
import Control.Arrow
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State hiding (state)
import Converter
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text, empty, pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (UTCTime, getCurrentTime)
import FeedConfig
import FeedReader
import Sanitizer
import Types
import Util

type ReadFlag = Bool

type PostMap = Map.Map Text Post

data UpdateEnv = UpdateEnv
  { _apiHost :: Api.ApiHost
  , _readFlag :: ReadFlag
  } deriving (Eq, Show)

data UpdateState = UpdateState
  { _updateTime :: Maybe UTCTime
  , _updateFeed :: Feed
  , _existingPosts :: PostMap
  , _latestPosts :: [Post]
  } deriving (Eq, Show)

makeLenses ''UpdateEnv

makeLenses ''UpdateState

type UpdateM a = ReaderT UpdateEnv (ExceptT SomeException (StateT UpdateState IO)) a

-- |
-- Convert an IO action into an UpdateM action
wrapM :: IO (Either SomeException a) -> UpdateM a
wrapM action = do
  result <- liftIO action
  either throwError return result

-- |
-- Run an UpdateM action
runUpdateM
  :: UpdateEnv
  -> UpdateState
  -> UpdateM a
  -> IO (Either SomeException a, UpdateState)
runUpdateM env st ev = runStateT (runExceptT (runReaderT ev env)) st

-- |
-- Create an initial state
nullState :: Feed -> UpdateState
nullState feed = UpdateState Nothing feed Map.empty []

-- |
-- Create an environment to be used when updating all feeds
envForUpdate :: String -> UpdateEnv
envForUpdate host = UpdateEnv host False

-- |
-- Create an environment to be used when adding a new feed
envForAddNew :: String -> UpdateEnv
envForAddNew host = UpdateEnv host True

-- |
-- Updates a feed by reading the latest posts, saving them to a persistent
-- store and indexing them
update :: UpdateEnv -> Feed -> IO (Either SomeException (Feed, [Post]))
update env feed = do
  (result, state) <- runUpdateM env (nullState feed) updateM
  case result of
    Left e -> saveFailedFeed env state e >> return (Left e)
    Right _ -> return $ Right (state ^. updateFeed, state ^. latestPosts)

updateM :: UpdateM ()
updateM = do
  _ <- initTime
  _ <- readExistingPosts
  _ <- fetchFeedData
  _ <- fetchPostsContent
  _ <- saveFeed
  _ <- processPosts
  _ <- indexPosts
  return ()

saveFailedFeed :: UpdateEnv
               -> UpdateState
               -> SomeException
               -> IO (Either SomeException Feed)
saveFailedFeed env state err =
  Api.saveFeed
    (env ^. apiHost)
    feed
    { feedFailedAttempts = 1 + feedFailedAttempts feed
    , feedLastReadDate = formatJsDate <$> _updateTime state
    , feedLastReadStatus = Just $ ReadFailure (pack $ show err)
    }
  where
    feed = _updateFeed state

readExistingPosts :: UpdateM ()
readExistingPosts = do
  host <- _apiHost <$> ask
  feed <- gets _updateFeed
  case feedId feed of
    Nothing -> existingPosts .= Map.empty
    Just fid -> do
      posts <- wrapM $ Api.fetchPosts host fid
      existingPosts .= toMap posts
  return ()
  where
    toMap = Map.fromList . fmap (postGuid &&& id)

processPosts :: UpdateM ()
processPosts = do
  host <- asks _apiHost
  feed <- gets _updateFeed
  posts <- gets _latestPosts
  saved <- wrapM $ Api.savePosts host (prepare feed <$> posts)
  latestPosts .= saved
  where
    prepare feed = setPostFeedId feed . sanitizePost feed
    setPostFeedId feed post =
      post
      { postFeedId = feedId feed
      }

sanitizePost :: Feed -> Post -> Post
sanitizePost feed post =
  post
  { postDescription = sanitizeHtml (postDescription post)
  }
  where
    feedUrl = unpack (feedUri feed)
    baseUrl = unpack (postLink post)
    sanitizeHtml html = pack . sanitize feedUrl baseUrl . unpack <$> html

indexPosts :: UpdateM ()
indexPosts = do
  host <- asks _apiHost
  fid <- gets (fromJust . feedId . _updateFeed)
  subs <- wrapM $ Api.fetchSubscriptions host fid
  posts <- gets _latestPosts
  markAsRead <- asks _readFlag
  void $ wrapM $ try (mapConcurrently (idx host posts markAsRead) subs)
  where
    idx = Api.indexPosts

fetchFeedData :: UpdateM ()
fetchFeedData = do
  feed <- gets _updateFeed
  fdata <- wrapM $ fetchFeed (unpack $ feedUri feed) (modified feed)
  processFeed fdata
  where
    modified feed = fromMaybe nullLastModified (feedLastModified feed)

saveFeed :: UpdateM ()
saveFeed = do
  feed <- gets _updateFeed
  host <- asks _apiHost
  saved <- wrapM $ Api.saveFeed host feed
  updateFeed .= saved

initTime :: UpdateM ()
initTime = do
  time <- liftIO getCurrentTime
  updateTime .= Just time

fetchPostsContent :: UpdateM ()
fetchPostsContent = do
  uri <- gets (unpack . feedUri . _updateFeed)
  latest <- gets _latestPosts
  when (inlineRequired uri) $
    do posts <- wrapM $ fetchContent latest
       latestPosts .= posts

fetchContent :: [Post] -> IO (Either SomeException [Post])
fetchContent posts = fmap (fmap updatePost . zip posts) <$> run
  where
    run :: IO (Either SomeException [Either SomeException ByteString])
    run = try (mapConcurrently fetchPost links)
    links = unpack . postLink <$> posts
    updatePost (post, Left e) =
      post
      { postInlineStatus = Just $ ReadFailure (pack $ show e)
      }
    updatePost (post, Right b) =
      post
      { postDescription = Just (toStrict $ decodeUtf8 b)
      , postInlineStatus = Just ReadSuccess
      }

processFeed :: (ByteString, LastModified) -> UpdateM ()
processFeed (bytes, modified) = do
  origFeed <- gets _updateFeed
  existing <- gets _existingPosts
  time <- gets _updateTime
  results <- wrapM . return $ extractFeedAndPosts bytes
  let posts = filter (isNew $ Map.keysSet existing) (snd results)
      (nextPosts, dates) = sequenceT $ normalizePostDates (fromJust time) <$> posts
      lastDate = formatJsDate <$> maxDate (catMaybes dates)
      feed = fst results
  latestPosts .= nextPosts
  updateFeed .=
    feed
    { feedFailedAttempts = 0
    , feedGuid = feedGuid origFeed <|> feedGuid feed
    , feedId = feedId origFeed
    , feedLastModified = Just modified
    , feedLastPostDate = lastDate <|> feedLastPostDate origFeed
    , feedLastReadDate = formatJsDate <$> time
    , feedLastReadStatus = Just ReadSuccess
    , feedLink = feedGuid feed <|> feedGuid origFeed
    , feedPostCount = Map.size existing + length nextPosts
    , feedUri = feedUri origFeed
    }
  where
    isNew guids post = not $ Set.member (postGuid post) guids
    sequenceT = fmap fst &&& fmap snd
    maxDate [] = Nothing
    maxDate xs = Just (maximum xs)

normalizePostDates :: UTCTime -> Post -> (Post, Maybe UTCTime)
normalizePostDates now post =
  ( post
    { postDate = t1 <|> postDate post
    , postPubdate = t2 <|> postPubdate post
    }
  , d1)
  where
    (t1, d1) = normalizeDate now (fromMaybe empty $ postDate post)
    (t2, _) = normalizeDate now (fromMaybe empty $ postPubdate post)
