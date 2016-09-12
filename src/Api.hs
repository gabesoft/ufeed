{-# LANGUAGE OverloadedStrings #-}

-- |
-- Interaction with the api server
module Api where

import Control.Exception (SomeException, try)
import Control.Lens ((&), (.~), (^.))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Aeson.Types (Value)
import Data.Text (pack, unpack)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import Network.Wreq
import Network.Wreq.Types
import Types

data PostSearchParams = PostSearchParams
  { postFields :: [String]
  , queryFeedId :: String
  } deriving (Eq, Show)

instance ToJSON PostSearchParams where
  toJSON p =
    object
      [ "fields" .= unwords (postFields p)
      , "query" .= object ["feedId" .= queryFeedId p]
      ]

-- |
-- Get a number of feeds from the api server according to a specified limit
-- A value of 0 for limit causes all feeds to be returned
fetchFeeds :: String -> Int -> IO (Either SomeException [Feed])
fetchFeeds host limit = try (fetchFeeds' host limit)

-- |
-- Get all posts for a feed specified by a feed id
fetchPosts :: String -> String -> IO (Either SomeException [Post])
fetchPosts host fid = try (fetchPosts' host fid)

-- |
-- Save a new feed or update an existing feed
saveFeed :: String -> Feed -> IO (Either SomeException Feed)
saveFeed host feed =
  case feedId feed of
    Nothing -> try (postFeed host feed)
    Just _ -> try (patchFeed host feed)

-- |
-- Save a new feed
postFeed :: String -> Feed -> IO Feed
postFeed host = saveFeed' post (host ++ "/feeds")

-- |
-- Update an existing feed
patchFeed :: String -> Feed -> IO Feed
patchFeed host feed = saveFeed' (customPayloadMethod "PATCH") url feed
  where
    url = host ++ "/feeds" ++ "/" ++ (unpack . fromJust) (feedId feed)

saveFeed' :: (String -> Value -> IO (Response ByteString))
          -> String
          -> Feed
          -> IO Feed
saveFeed' method url feed =
  (^. responseBody) <$> (method url (toJSON feed) >>= asJSON)

fetchFeeds' :: String -> Int -> IO [Feed]
fetchFeeds' host limit = (^. responseBody) <$> (getWith opts url >>= asJSON)
  where
    url = host ++ "/search/feeds"
    opts = defaults & param "limit" .~ [pack $ show limit]

fetchPosts' :: String -> String -> IO [Post]
fetchPosts' host fid =
  (^. responseBody) <$> (post url (toJSON query) >>= asJSON)
  where
    url = host ++ "/search/posts"
    fields = ["guid", "date", "link"]
    query = PostSearchParams fields fid
