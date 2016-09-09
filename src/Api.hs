{-# LANGUAGE OverloadedStrings #-}

-- |
-- Interaction with the api server
module Api where

import Control.Lens
import Data.Text (pack)
import Types
import Network.Wreq

-- |
-- Get a number of feeds from the api server according to a specified limit
-- A value of 0 for limit causes all feeds to be returned
fetchFeeds :: String -> Int -> IO [Feed]
fetchFeeds host limit = (^. responseBody) <$> (getWith opts url >>= asJSON)
  where url = host ++ "/search/feeds"
        opts = defaults & param "limit" .~ [pack $ show limit]

-- |
-- Get all posts for a feed specified by a feed id
fetchPosts :: String -> String -> IO [Post]
fetchPosts host feedId = undefined