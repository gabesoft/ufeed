{-# LANGUAGE OverloadedStrings #-}

-- |
-- Interaction with the api server
module Api where

import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Aeson.Types (Value)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (void)
import Data.Maybe
import Data.Text (pack, unpack)
import Network.Wreq
import Types

data SearchParams = SearchParams
  { postFields :: [String]
  , queryFeedId :: String
  } deriving (Eq, Show)

data IndexParams = IndexParams
  { indexPostIds :: [String]
  , indexReadFlag :: Bool
  } deriving (Eq, Show)

instance ToJSON SearchParams where
  toJSON p =
    object
      [ "fields" .= unwords (postFields p)
      , "query" .= object ["feedId" .= queryFeedId p]
      ]

instance ToJSON IndexParams where
  toJSON p =
    object
      [ "postIds" .= indexPostIds p
      , "data" .= object ["read" .= indexReadFlag p]
      ]

-- |
-- Get all subscriptions for a feed
fetchSubscriptions :: String
                   -> String
                   -> IO (Either SomeException [FeedSubscription])
fetchSubscriptions host feedId = try (fetchSubscriptions' host feedId)

-- |
-- Index a list of posts
indexPosts :: String
           -> [Post]
           -> Bool
           -> FeedSubscription
           -> IO (Either SomeException ())
indexPosts host posts readFlag subscription =
  try (indexPosts' host posts readFlag subscription)

-- |
-- Get a number of feeds from the api server according to a specified limit
-- A value of 0 for limit causes all feeds to be returned
fetchFeeds :: String -> Int -> IO (Either SomeException [Feed])
fetchFeeds host limit = try (fetchFeeds' host limit)

-- |
-- Get all posts for a feed specified by a feed id
fetchPosts :: String -> String -> IO (Either SomeException [Post])
fetchPosts host feedId = try (fetchPosts' host feedId)

-- |
-- Save a new feed or update an existing feed
saveFeed :: String -> Feed -> IO (Either SomeException Feed)
saveFeed host feed =
  case feedId feed of
    Nothing -> try (postFeed host feed)
    Just _ -> try (patchFeed host feed)

-- |
-- Save a list of posts
savePosts :: String -> [Post] -> IO (Either SomeException [Post])
savePosts host posts = try (savePosts' host posts)

-- |
-- Get a feed by id
fetchFeed :: String -> String -> IO (Either SomeException Feed)
fetchFeed host feedId = try (fetchFeed' host feedId)

-- |
-- Save a new feed
postFeed :: String -> Feed -> IO Feed
postFeed host = saveFeed' post (host ++ "/feeds")

-- |
-- Update an existing feed
patchFeed :: String -> Feed -> IO Feed
patchFeed host feed = saveFeed' (customPayloadMethod "PATCH") url feed
  where
    url = host ++ "/feeds/" ++ (unpack . fromJust) (feedId feed)

saveFeed' :: (String -> Value -> IO (Response ByteString))
          -> String
          -> Feed
          -> IO Feed
saveFeed' method url feed =
  (^. responseBody) <$> (method url (toJSON feed) >>= asJSON)

savePosts' :: String -> [Post] -> IO [Post]
savePosts' host posts = do
  saved <- (^. responseBody) <$> (post url (toJSON posts) >>= asJSON)
  return $ filter (isJust . postId) saved
  where
    url = host ++ "/bulk/posts"

fetchSubscriptions' :: String -> String -> IO [FeedSubscription]
fetchSubscriptions' host feedId =
  (^. responseBody) <$> (post url (toJSON query) >>= asJSON)
  where
    url = host ++ "/search/feed-subscriptions"
    fields = ["feedId"]
    query = SearchParams fields feedId

fetchFeed' :: String -> String -> IO Feed
fetchFeed' host feedId = (^. responseBody) <$> (get url >>= asJSON)
  where
    url = host ++ "/feeds/" ++ feedId

fetchFeeds' :: String -> Int -> IO [Feed]
fetchFeeds' host limit = (^. responseBody) <$> (getWith opts url >>= asJSON)
  where
    url = host ++ "/search/feeds"
    opts = defaults & param "limit" .~ [pack $ show limit]

fetchPosts' :: String -> String -> IO [Post]
fetchPosts' host feedId =
  (^. responseBody) <$> (post url (toJSON query) >>= asJSON)
  where
    url = host ++ "/search/posts"
    fields = ["guid", "date", "link"]
    query = SearchParams fields feedId

indexPosts' :: String -> [Post] -> Bool -> FeedSubscription -> IO ()
indexPosts' host posts readFlag subscription = void $ post url (toJSON args)
  where
    url = host ++ "/bulk/user-posts/" ++ unpack (subscriptionId subscription)
    ids = unpack . fromJust . postId <$> posts
    args = IndexParams ids readFlag
