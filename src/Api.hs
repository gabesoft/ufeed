{-# LANGUAGE OverloadedStrings #-}

-- |
-- Interaction with the api server
module Api where

import Control.Exception (SomeException, try)
import Control.Lens ((^.), (&), (.~))
import Data.Aeson (toJSON, ToJSON,object,(.=))
import Data.Text (pack)
import Network.Wreq
import Types

data PostSearchParams =
  PostSearchParams {postFields :: [String]
                   ,queryFeedId :: String}
  deriving (Eq,Show)

instance ToJSON PostSearchParams where
  toJSON p =
    object ["fields" .= unwords (postFields p)
           ,"query" .= object ["feedId" .= queryFeedId p]]

-- |
-- Get a number of feeds from the api server according to a specified limit
-- A value of 0 for limit causes all feeds to be returned
fetchFeeds
  :: String -> Int -> IO (Either SomeException [Feed])
fetchFeeds host limit = try (fetchFeeds' host limit)

-- |
-- Get all posts for a feed specified by a feed id
fetchPosts
  :: String -> String -> IO (Either SomeException [Post])
fetchPosts host fid = try (fetchPosts' host fid)

fetchFeeds' :: String -> Int -> IO [Feed]
fetchFeeds' host limit = (^. responseBody) <$> (getWith opts url >>= asJSON)
  where url = host ++ "/search/feeds"
        opts = defaults & param "limit" .~ [pack $ show limit]

fetchPosts' :: String -> String -> IO [Post]
fetchPosts' host fid =
  (^. responseBody) <$> (post url (toJSON query) >>= asJSON)
  where url = host ++ "/search/posts"
        fields = ["guid","date","link"]
        query = PostSearchParams fields fid
