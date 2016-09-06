-- | Functions for fetching feeds data
module FeedReader where

import           Control.Exception         (SomeException, try)
import           Control.Lens              ((&), (.~), (^.), (^?))
import           Data.ByteString.Lazy      (ByteString)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 as T (empty)
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import           Network.HTTP.Types.Header
import           Network.Wreq
import           Types

-- |
-- Fetch a feed and all its posts that are newer than
-- the specified last modified information
fetchFeed
  :: String
  -> LastModified
  -> IO (Either SomeException (ByteString,LastModified))
fetchFeed uri modified = try (fetchFeed' uri modified)

-- |
-- Add the last modified headers to the default options
modifiedHeaders
  :: Options -> LastModified -> Options
modifiedHeaders opts modified = foldr step opts (fs <*> [modified])
  where fs = [(,) hIfNoneMatch . etag,(,) hIfModifiedSince . lastModified]
        step (h,v) d = d & header h .~ [(encodeUtf8 . fromMaybe empty) v]

fetchFeed'
  :: String -> LastModified -> IO (ByteString,LastModified)
fetchFeed' uri modified =
  do res <- getWith (modifiedHeaders defaults modified) uri
     let body = res ^. responseBody
     let et = res ^? responseHeader hETag
     let lm = res ^? responseHeader hLastModified
     return (body
            ,LastModified (decodeUtf8 <$> et)
                          (decodeUtf8 <$> lm))