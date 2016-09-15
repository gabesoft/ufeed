-- |
-- Updates a feed
-- Updating a feed consists of fetching the latest feed data,
-- uploading the updated feed and any new posts to database,
-- and indexing the new posts with the search engine
module FeedUpdater where

import qualified Api as Api
import Control.Applicative ((<|>))
import Control.Arrow
import Control.Concurrent.Async
import Control.Exception
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
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

type ApiHost = String

type ReadFlag = Bool

type PostMap = Map.Map Text Post

data UpdateEnv = UpdateEnv
  { apiHost :: String
  , readFlag :: Bool
  } deriving (Eq, Show)

data UpdateState = UpdateState
  { updateTime :: UTCTime
  , updateFeed :: Feed
  , existingPosts :: PostMap
  , latestPosts :: [Post]
  } deriving (Eq, Show)

-- TODO: find a better name for this
--       also rename all ..T methods
type UpdateT = ExceptT SomeException IO UpdateState

-- |
-- Creates an environment to be used when updating all feeds
envForUpdate :: String -> UpdateEnv
envForUpdate host = UpdateEnv host False

-- |
-- Creates an environment to be used when adding a new feed
envForAddNew :: String -> UpdateEnv
envForAddNew host = UpdateEnv host True

updateT :: UpdateEnv -> Feed -> IO (Either SomeException (Feed, [Post]))
updateT env feed = do
  st0 <- initStateT feed
  st1 <- runExceptT $ processFeedT env st0
  case st1 of
    Left e -> fmap (flip (,) []) <$> saveFailedFeed env st0 e
    Right st2 ->
      fmap results <$> runExceptT (processPostsT env st2 >>= indexPostsT env)
  where
    results state = (updateFeed state, latestPosts state)

indexPostsT :: UpdateEnv -> UpdateState -> UpdateT
indexPostsT = undefined

initStateT :: Feed -> IO UpdateState
initStateT feed = do
  time <- liftIO getCurrentTime
  return $ UpdateState time feed Map.empty []

saveFailedFeed :: UpdateEnv
               -> UpdateState
               -> SomeException
               -> IO (Either SomeException Feed)
saveFailedFeed env state err =
  Api.saveFeed
    (apiHost env)
    feed
    { feedFailedAttempts = 1 + feedFailedAttempts feed
    , feedLastReadDate = Just (formatJsDate time)
    , feedLastReadStatus = Just $ ReadFailure (pack $ show err)
    }
  where
    feed = updateFeed state
    time = updateTime state

saveFeedT :: UpdateEnv -> UpdateState -> UpdateT
saveFeedT env state = do
  feed <- ExceptT $ Api.saveFeed (apiHost env) (updateFeed state)
  return $
    state
    { updateFeed = feed
    }

processPostsT :: UpdateEnv -> UpdateState -> UpdateT
processPostsT env state = undefined

processFeedT :: UpdateEnv -> UpdateState -> UpdateT
processFeedT env state =
  readExistingPostsT env state >>= fetchFeedDataT >>= fetchPostsContentT >>=
  saveFeedT env

readExistingPostsT :: UpdateEnv -> UpdateState -> UpdateT
readExistingPostsT env state =
  case feedId (updateFeed state) of
    Nothing -> return $ set Map.empty
    Just fid -> do
      posts <- ExceptT $ Api.fetchPosts host (unpack fid)
      return $ set . Map.fromList . fmap (postGuid &&& id) $ posts
  where
    host = apiHost env
    set e =
      state
      { existingPosts = e
      }

fetchFeedDataT :: UpdateState -> UpdateT
fetchFeedDataT state = do
  fd <- liftIO $ fetchFeed (unpack $ feedUri feed) modified
  ExceptT $ return (fd >>= processFeed state)
  where
    feed = updateFeed state
    modified = fromMaybe nullLastModified (feedLastModified feed)

fetchPostsContentT :: UpdateState -> UpdateT
fetchPostsContentT state =
  if inlineRequired $ unpack (feedUri $ updateFeed state)
    then ExceptT $ fmap set <$> fetchContent (latestPosts state)
    else return state
  where
    set posts =
      state
      { latestPosts = posts
      }

--------------------------------------------------------------------------------
-- Monad stack : StateT EitherT IO
update :: UpdateEnv -> Feed -> IO (Either SomeException (Feed, [Post]))
update env feed = do
  ut <- getCurrentTime
  ep <- readExistingPosts (apiHost env) (unpack <$> feedId feed)
  case ep of
    Left e -> return $ Left e
    Right posts -> do
      st <- fetchFeedData (UpdateState ut feed posts [])
      case st of
        Left e -> do
          _ <-
            Api.saveFeed
              (apiHost env)
              (feed
               { feedFailedAttempts = 1 + feedFailedAttempts feed
               , feedLastReadDate = Just (formatJsDate ut)
               , feedLastReadStatus = Just $ ReadFailure (pack $ show e)
               })
          return $ Left e
        Right s -> do
          updatedSt <- fetchPostsContent s
          case updatedSt of
            Left e -> return $ Left e
            Right s' -> do
              let st' = sanitizePosts s'
              saved <- Api.saveFeed (apiHost env) (updateFeed st')
              case saved of
                Left e -> return $ Left e
                Right fs -> do
                  let ps = updateFeedId fs <$> latestPosts st'
                  ps' <- Api.savePosts (apiHost env) ps
                  case ps' of
                    Left e -> return $ Left e
                    Right ps'' -> do
                      let st'' =
                            st'
                            { updateFeed = fs
                            , latestPosts = ps''
                            }
                      return $ Right (fs, latestPosts st'')

readExistingPosts :: String -> Maybe String -> IO (Either SomeException PostMap)
readExistingPosts _ Nothing = return (Right Map.empty)
readExistingPosts host (Just fid) = do
  posts <- Api.fetchPosts host fid
  return $ Map.fromList . fmap (postGuid &&& id) <$> posts

-- TODO: fetch subscriptions
--       index feed if it has new posts
updateFeedId :: Feed -> Post -> Post
updateFeedId feed post =
  post
  { postFeedId = feedId feed
  }

sanitizePosts :: UpdateState -> UpdateState
sanitizePosts state =
  state
  { latestPosts = sanitizePost (updateFeed state) <$> latestPosts state
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

fetchFeedData :: UpdateState -> IO (Either SomeException UpdateState)
fetchFeedData state = do
  dt <- fetchFeed (unpack $ feedUri feed) modified
  return (dt >>= processFeed state)
  where
    feed = updateFeed state
    modified = fromMaybe nullLastModified (feedLastModified feed)

fetchPostsContent :: UpdateState -> IO (Either SomeException UpdateState)
fetchPostsContent state =
  if inlineRequired $ unpack (feedUri $ updateFeed state)
    then fmap updateState <$> fetchContent (latestPosts state)
    else return (Right state)
  where
    updateState posts =
      state
      { latestPosts = posts
      }

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

processFeed :: UpdateState
            -> (ByteString, LastModified)
            -> Either SomeException UpdateState
processFeed state (bytes, modified) = do
  (newFeed, ps) <- extractFeedAndPosts bytes
  let newPosts = filter (isNew $ Map.keysSet existing) ps
      (posts, dates) = sequenceT $ normalizePostDates (updateTime state) <$> newPosts
      lastDate = formatJsDate <$> maximumDate (catMaybes dates)
  return
    state
    { latestPosts = posts
    , updateFeed =
      newFeed
      { feedFailedAttempts = 0
      , feedGuid = feedGuid feed <|> feedGuid newFeed
      , feedId = feedId feed
      , feedLastModified = Just modified
      , feedLastPostDate = lastDate <|> feedLastPostDate feed
      , feedLastReadDate = Just $ formatJsDate (updateTime state)
      , feedLastReadStatus = Just ReadSuccess
      , feedLink = feedGuid newFeed <|> feedGuid feed
      , feedPostCount = Map.size existing + length posts
      , feedUri = feedUri feed
      }
    }
  where
    feed = updateFeed state
    existing = existingPosts state
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
