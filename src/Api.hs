{-# LANGUAGE OverloadedStrings #-}

-- ^
-- Interaction with the api server
module Api where

import Control.Exception
       (Exception, SomeException, try, toException, throw, fromException)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Aeson.Types (Value)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (void)
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, append)
import Network.Wreq
import Types

type ApiHost = String

newtype SearchException =
  NoResultsException String
  deriving (Eq, Ord, Show)

instance Exception SearchException

maxResultsSize :: Int
maxResultsSize = 1048576

-- ^
-- Get all subscriptions for a feed
fetchSubscriptions :: ApiHost
                   -> Text
                   -> IO (Either SomeException [FeedSubscription])
fetchSubscriptions host feedId = try (fetchSubscriptions' host feedId)

-- ^
-- Index a list of posts
indexPosts :: ApiHost
           -> [Post]
           -> Bool
           -> FeedSubscription
           -> IO (Either SomeException ())
indexPosts host posts readFlag subscription =
  try (indexPosts' host posts readFlag subscription)

-- ^
-- Get a number of feeds from the api server according to a specified limit
-- A value of 0 for limit causes all feeds to be returned
fetchFeeds :: ApiHost -> IO (Either SomeException [Feed])
fetchFeeds host = try (fetchFeeds' host)

-- ^
-- Get all posts for a feed specified by a feed id
fetchPosts :: ApiHost -> Text -> IO (Either SomeException [Post])
fetchPosts host feedId = try (fetchPosts' host feedId)

-- ^
-- Save a new feed or update an existing feed
saveFeed :: ApiHost -> Feed -> IO (Either SomeException Feed)
saveFeed host feed =
  case feedId feed of
    Nothing -> try (postFeed host feed)
    Just _ -> try (patchFeed host feed)

-- ^
-- Save a list of posts
savePosts :: ApiHost -> [Post] -> IO (Either SomeException [Post])
savePosts host posts = try (savePosts' host posts)

-- ^
-- Get a feed by id
fetchFeed :: ApiHost -> Text -> IO (Either SomeException Feed)
fetchFeed host feedId = try (fetchFeed' host feedId)

-- ^
-- Get a feed by uri
fetchFeedByUri :: ApiHost -> Text -> IO (Either SomeException Feed)
fetchFeedByUri host uri = do
  feeds <- try (fetchFeedByUri' host uri)
  return $
    case feeds of
      Left e -> Left e
      Right [] ->
        Left $ mkNoResultsError ("No feed found matching uri " <> unpack uri)
      Right (x:_) -> Right x

-- ^
-- Save a new feed
postFeed :: ApiHost -> Feed -> IO Feed
postFeed host = saveFeed' post (host ++ "/xandar/feeds")

-- ^
-- Update an existing feed
patchFeed :: ApiHost -> Feed -> IO Feed
patchFeed host feed = saveFeed' (customPayloadMethod "PATCH") url feed
  where
    url = host <> "/xandar/feeds/" <> (unpack . fromJust) (feedId feed)

saveFeed' :: (String -> Value -> IO (Response ByteString))
          -> String
          -> Feed
          -> IO Feed
saveFeed' method url feed =
  (^. responseBody) <$> (method url (toJSON feed) >>= asJSON)

savePosts' :: ApiHost -> [Post] -> IO [Post]
savePosts' host posts = do
  saved <- (^. responseBody) <$> (post url (toJSON posts) >>= asJSON)
  return $ filter (isJust . postId) saved
  where
    url = host <> "/xandar/posts"

fetchSubscriptions' :: ApiHost -> Text -> IO [FeedSubscription]
fetchSubscriptions' host feedId =
  (^. responseBody) <$> (getWith opts url >>= asJSON)
  where
    url = host <> "/xandar/subscriptions"
    fields = param "include" .~ ["feedId"]
    query = param "where" .~ ["(feedId eq " <> feedId <> ")"]
    opts = defaults & query & fields

fetchFeed' :: ApiHost -> Text -> IO Feed
fetchFeed' host feedId = (^. responseBody) <$> (get url >>= asJSON)
  where
    url = host <> "/xandar/feeds/" <> unpack feedId

fetchFeedByUri' :: ApiHost -> Text -> IO [Feed]
fetchFeedByUri' host uri = (^. responseBody) <$> (getWith opts url >>= asJSON)
  where
    url = host <> "/xandar/feeds"
    query = param "where" .~ ["(uri eq '" <> uri <> "')"]
    opts = defaults & query

fetchFeeds' :: ApiHost -> IO [Feed]
fetchFeeds' host = (^. responseBody) <$> (getWith opts url >>= asJSON)
  where
    url = host <> "/xandar/feeds"
    opts = defaults & param "per_page" .~ [pack $ show maxResultsSize]

fetchPosts' :: ApiHost -> Text -> IO [Post]
fetchPosts' host feedId = (^. responseBody) <$> (getWith opts url >>= asJSON)
  where
    url = host <> "/xandar/posts"
    fields = param "include" .~ ["guid", "date", "link"]
    perPage = param "per_page" .~ [pack $ show maxResultsSize]
    query = param "where" .~ ["(feedId eq " <> feedId <> ")"]
    opts = defaults & query & fields & perPage

indexPosts' :: ApiHost -> [Post] -> Bool -> FeedSubscription -> IO ()
indexPosts' host posts readFlag subscription = void $ post url (toJSON args)
  where
    url = host <> "/xandar/user-posts"
    ids = fromJust . postId <$> posts
    subId = subscriptionId subscription
    args = UserPost subId readFlag <$> ids

mkNoResultsError :: String -> SomeException
mkNoResultsError msg = toException (NoResultsException msg)

isNoResultsError :: SomeException -> Bool
isNoResultsError err =
  case (fromException err :: Maybe Api.SearchException) of
    Just (Api.NoResultsException _) -> True
    _ -> False
