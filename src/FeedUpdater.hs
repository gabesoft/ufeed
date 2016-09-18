{-# LANGUAGE TemplateHaskell #-}

-- |
-- Updates a feed
-- Updating a feed consists of fetching the latest feed data,
-- uploading the updated feed and any new posts to database,
-- and indexing the new posts with the search engine
module FeedUpdater where

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
import Data.Time.Format
       (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.Time.ISO8601 (formatISO8601Javascript, parseISO8601)
import FeedConfig
import FeedReader
import Sanitizer
import Types

type ApiHost = String

type ReadFlag = Bool

type PostMap = Map.Map Text Post

data UpdateEnv = UpdateEnv
  { _apiHost :: String
  , _readFlag :: Bool
  } deriving (Eq, Show)

makeLenses ''UpdateEnv

data UpdateState = UpdateState
  { _updateTime :: Maybe UTCTime
  , _updateFeed :: Feed
  , _existingPosts :: PostMap
  , _latestPosts :: [Post]
  } deriving (Eq, Show)

makeLenses ''UpdateState

type UpdateM a = ReaderT UpdateEnv (ExceptT SomeException (StateT UpdateState IO)) a

-- |
-- Convert an IO action into an UpdateM action
wrapM :: IO (Either SomeException a) -> UpdateM a
wrapM action = do
  result <- liftIO action
  either throw return result

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
    , feedLastReadDate = formatJsDate <$> time
    , feedLastReadStatus = Just $ ReadFailure (pack $ show err)
    }
  where
    feed = _updateFeed state
    time = _updateTime state

readExistingPosts :: UpdateM ()
readExistingPosts = do
  host <- _apiHost <$> ask
  feed <- gets _updateFeed
  case feedId feed of
    Nothing -> modify (existingPosts .~ Map.empty)
    Just fid -> do
      posts <- wrapM $ Api.fetchPosts host (unpack fid)
      modify (existingPosts .~ toMap posts)
  return ()
  where
    toMap = Map.fromList . fmap (postGuid &&& id)

processPosts :: UpdateM ()
processPosts = do
  host <- asks _apiHost
  feed <- gets _updateFeed
  _ <- modify (updatePostsFeedId feed . sanitizePosts)
  posts <- gets _latestPosts
  saved <- wrapM $ Api.savePosts host posts
  modify (latestPosts .~ saved)
  where
    updatePostsFeedId feed = latestPosts %~ fmap (setPostFeedId feed)
    setPostFeedId feed post =
      post
      { postFeedId = feedId feed
      }

indexPosts :: UpdateM ()
indexPosts = do
  host <- asks _apiHost
  fid <- gets (unpack . fromJust . feedId . _updateFeed)
  subs <- wrapM $ Api.fetchSubscriptions host fid
  posts <- gets _latestPosts
  rf <- asks _readFlag
  void $ wrapM $ try (mapConcurrently (Api.indexPosts host posts rf) subs)

fetchFeedData :: UpdateM ()
fetchFeedData = do
  feed <- gets _updateFeed
  fdata <- wrapM $ fetchFeed (unpack $ feedUri feed) (modified feed)
  processFeed fdata
  where
    modified feed = fromMaybe nullLastModified (feedLastModified feed)

fetchPostsContent :: UpdateM ()
fetchPostsContent = do
  uri <- gets (unpack . feedUri . _updateFeed)
  latest <- gets _latestPosts
  when (inlineRequired uri) $
    do posts <- wrapM $ fetchContent latest
       modify (set posts)
  where
    set p s =
      s
      { _latestPosts = p
      }

saveFeed :: UpdateM ()
saveFeed = do
  feed <- gets _updateFeed
  host <- asks _apiHost
  saved <- wrapM $ Api.saveFeed host feed
  modify (set saved)
  where
    set f s =
      s
      { _updateFeed = f
      }

initTime :: UpdateM ()
initTime = do
  time <- liftIO getCurrentTime
  modify (set time)
  return ()
  where
    set time st =
      st
      { _updateTime = Just time
      }

initState :: Feed -> IO UpdateState
initState feed = do
  time <- getCurrentTime
  return (UpdateState (Just time) feed Map.empty [])

sanitizePosts :: UpdateState -> UpdateState
sanitizePosts state =
  state
  { _latestPosts = sanitizePost (_updateFeed state) <$> _latestPosts state
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
  state <- get
  (newFeed, ps) <- wrapM . return $ extractFeedAndPosts bytes
  let feed = _updateFeed state
      existing = _existingPosts state
      newPosts = filter (isNew $ Map.keysSet existing) ps
      (posts, dates) =
        sequenceT $ normalizePostDates (fromJust $ _updateTime state) <$> newPosts
      lastDate = formatJsDate <$> maximumDate (catMaybes dates)
  modify $
    const
      state
      { _latestPosts = posts
      , _updateFeed =
        newFeed
        { feedFailedAttempts = 0
        , feedGuid = feedGuid feed <|> feedGuid newFeed
        , feedId = feedId feed
        , feedLastModified = Just modified
        , feedLastPostDate = lastDate <|> feedLastPostDate feed
        , feedLastReadDate = formatJsDate <$> _updateTime state
        , feedLastReadStatus = Just ReadSuccess
        , feedLink = feedGuid newFeed <|> feedGuid feed
        , feedPostCount = Map.size existing + length posts
        , feedUri = feedUri feed
        }
      }
  where
    isNew guids post = not $ Set.member (postGuid post) guids
    sequenceT = fmap fst &&& fmap snd

maximumDate :: [UTCTime] -> Maybe UTCTime
maximumDate [] = Nothing
maximumDate xs = Just (maximum xs)

formatJsDate :: UTCTime -> Text
formatJsDate = pack . formatISO8601Javascript

normalizePostDates :: UTCTime -> Post -> (Post, Maybe UTCTime)
normalizePostDates now post =
  ( post
    { postDate = fromJust (t1 <|> Just (postDate post))
    , postPubdate = t2 <|> postPubdate post
    }
  , d1)
  where
    (t1, d1) = normalizeDate now (postDate post)
    (t2, _) = normalizeDate now (fromMaybe empty (postPubdate post))

normalizeDate :: UTCTime -> Text -> (Maybe Text, Maybe UTCTime)
normalizeDate now input = (formatJsDate <$> date, date)
  where
    date = (`min` now) <$> parseJsDate (unpack input)

parseJsDate :: String -> Maybe UTCTime
parseJsDate input = parseISO8601 input <|> parseRFC822 input

parseRFC822 :: String -> Maybe UTCTime
parseRFC822 = parseTimeM True defaultTimeLocale rfc822DateFormat
