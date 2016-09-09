-- |
-- Processor for a feed
-- Processing consists of fetching new posts and saving them to
-- a persistent store
module FeedProcessor where

import Api
import Types

process :: Feed -> IO ()
process = undefined