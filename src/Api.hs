{-# LANGUAGE OverloadedStrings #-}

-- |
-- Interaction with the api server
module Api where

import Control.Exception
       (Exception, SomeException, try, toException, throw, fromException)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Aeson.Types (Value)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (void)
import Data.Maybe
import Data.Text (Text, pack, unpack, append)
import Network.Wreq
import Types

type ApiHost = String

data SearchException =
  NoResultsException String
  deriving (Eq, Ord, Show)

instance Exception SearchException

data SearchParams = SearchParams
  { queryFields :: [Text]
  , queryFeedId :: Maybe Text
  , queryFeedUri :: Maybe Text
  } deriving (Eq, Show)

data IndexParams = IndexParams
  { indexPostIds :: [Text]
  , indexReadFlag :: Bool
  } deriving (Eq, Show)

instance ToJSON SearchParams where
  toJSON p =
    object
      [ "fields" .= queryFields p
      , "query" .= object ["feedId" .= queryFeedId p, "uri" .= queryFeedUri p]
      ]

instance ToJSON IndexParams where
  toJSON p =
    object
      [ "postIds" .= indexPostIds p
      , "data" .= object ["read" .= indexReadFlag p]
      ]

-- |
-- Get all subscriptions for a feed
fetchSubscriptions :: ApiHost
                   -> Text
                   -> IO (Either SomeException [FeedSubscription])
fetchSubscriptions host feedId = try (fetchSubscriptions' host feedId)

-- |
-- Index a list of posts
indexPosts :: ApiHost
           -> [Post]
           -> Bool
           -> FeedSubscription
           -> IO (Either SomeException ())
indexPosts host posts readFlag subscription =
  try (indexPosts' host posts readFlag subscription)

-- |
-- Get a number of feeds from the api server according to a specified limit
-- A value of 0 for limit causes all feeds to be returned
fetchFeeds :: ApiHost -> Int -> IO (Either SomeException [Feed])
fetchFeeds host limit = try (fetchFeeds' host limit)

-- |
-- Get all posts for a feed specified by a feed id
fetchPosts :: ApiHost -> Text -> IO (Either SomeException [Post])
fetchPosts host feedId = try (fetchPosts' host feedId)

-- |
-- Save a new feed or update an existing feed
saveFeed :: ApiHost -> Feed -> IO (Either SomeException Feed)
saveFeed host feed =
  case feedId feed of
    Nothing -> try (postFeed host feed)
    Just _ -> try (patchFeed host feed)

-- |
-- Save a list of posts
savePosts :: ApiHost -> [Post] -> IO (Either SomeException [Post])
savePosts host posts = try (savePosts' host posts)

-- |
-- Get a feed by id
fetchFeed :: ApiHost -> Text -> IO (Either SomeException Feed)
fetchFeed host feedId = try (fetchFeed' host feedId)

-- |
-- Get a feed by uri
fetchFeedByUri :: ApiHost -> Text -> IO (Either SomeException Feed)
fetchFeedByUri host uri = do
  feeds <- try (fetchFeedByUri' host uri)
  return $
    case feeds of
      Left e -> Left e
      Right [] ->
        Left $ mkNoResultsError ("No feed found matching uri " ++ unpack uri)
      Right (x:_) -> Right x

-- |
-- Save a new feed
postFeed :: ApiHost -> Feed -> IO Feed
postFeed host = saveFeed' post (host ++ "/feeds")

-- |
-- Update an existing feed
patchFeed :: ApiHost -> Feed -> IO Feed
patchFeed host feed = saveFeed' (customPayloadMethod "PATCH") url feed
  where
    url = host ++ "/feeds/" ++ (unpack . fromJust) (feedId feed)

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
    url = host ++ "/bulk/posts"

fetchSubscriptions' :: ApiHost -> Text -> IO [FeedSubscription]
fetchSubscriptions' host feedId =
  (^. responseBody) <$> (post url (toJSON query) >>= asJSON)
  where
    url = host ++ "/search/feed-subscriptions"
    fields = ["feedId"]
    query = SearchParams fields (Just feedId) Nothing

fetchFeed' :: ApiHost -> Text -> IO Feed
fetchFeed' host feedId = (^. responseBody) <$> (get url >>= asJSON)
  where
    url = host ++ "/feeds/" ++ unpack feedId

fetchFeedByUri' :: ApiHost -> Text -> IO [Feed]
fetchFeedByUri' host uri =
  (^. responseBody) <$> (post url (toJSON query) >>= asJSON)
  where
    url = host ++ "/search/feeds"
    query = SearchParams [] Nothing (Just uri)

fetchFeeds' :: ApiHost -> Int -> IO [Feed]
fetchFeeds' host limit = (^. responseBody) <$> (getWith opts url >>= asJSON)
  where
    url = host ++ "/search/feeds"
    opts = defaults & param "limit" .~ [pack $ show limit]

fetchPosts' :: ApiHost -> Text -> IO [Post]
fetchPosts' host feedId =
  (^. responseBody) <$> (post url (toJSON query) >>= asJSON)
  where
    url = host ++ "/search/posts"
    fields = ["guid", "date", "link"]
    query = SearchParams fields (Just feedId) Nothing

indexPosts' :: ApiHost -> [Post] -> Bool -> FeedSubscription -> IO ()
indexPosts' host posts readFlag subscription = void $ post url (toJSON args)
  where
    url = host ++ "/bulk/user-posts/" ++ unpack (subscriptionId subscription)
    ids = fromJust . postId <$> posts
    args = IndexParams ids readFlag

mkNoResultsError :: String -> SomeException
mkNoResultsError msg = toException (NoResultsException msg)

isNoResultsError :: SomeException -> Bool
isNoResultsError err =
  case (fromException err :: Maybe Api.SearchException) of
    Just (Api.NoResultsException _) -> True
    _ -> False
