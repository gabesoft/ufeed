-- |
-- Updates a feed
-- Updating a feed consists of fetching the latest feed data,
-- uploading the updated feed and any new posts to database,
-- and indexing the new posts with the search engine
module FeedProcessor where

import Api
import Types

process :: Feed -> IO ()
process = undefined
