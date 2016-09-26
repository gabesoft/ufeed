{-# LANGUAGE OverloadedStrings #-}

-- | Functions for fetching feeds data
module FeedReader where

import Control.Exception (SomeException, try)
import Network.Connection
import Control.Lens ((&), (.~), (^.), (^?))
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text as T (empty)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.Header
import Network.Wreq
import Network.HTTP.Client.TLS
import Types

-- |
-- Fetch a feed and all its posts that are newer than
-- the specified last modified information
fetchFeed :: String
          -> LastModified
          -> IO (Either SomeException (ByteString, LastModified))
fetchFeed uri modified = try (fetchFeed' uri modified)

-- |
-- Add the last modified headers to the default options
modifiedHeaders :: Options -> LastModified -> Options
modifiedHeaders opts modified = foldr step opts (fs <*> [modified])
  where
    fs = [(,) hIfNoneMatch . etag, (,) hIfModifiedSince . lastModified]
    step (h, v) d = d & header h .~ [(encodeUtf8 . fromMaybe empty) v]

-- | Fetch the html of a feed entry
fetchPost :: String -> IO (Either SomeException ByteString)
fetchPost uri = try $ (^. responseBody) <$> get uri

fetchFeed' :: String -> LastModified -> IO (ByteString, LastModified)
fetchFeed' uri modified = do
  res <- getWith (setTlsSettings $ modifiedHeaders defaults modified) uri
  let body = res ^. responseBody
      et = res ^? responseHeader hETag
      lm = res ^? responseHeader hLastModified
  return (body, LastModified (decodeUtf8 <$> et) (decodeUtf8 <$> lm))

addUserAgent :: Options -> Options
addUserAgent opts =
  opts & header hUserAgent .~
  [ "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.89 Safari/537.36"
  ]

setTlsSettings :: Options -> Options
setTlsSettings opts = opts & manager .~ Left tlsSettings

-- |
-- Disable certificate checking
-- https://github.com/snoyberg/http-client/issues/31
-- https://groups.google.com/forum/#!msg/yesodweb/7Lwzl2fvsZY/NjVpqGk1KlIJ
-- https://github.com/bos/wreq/blob/master/Network/Wreq/Internal.hs
-- https://hackage.haskell.org/package/http-client-tls-0.3.3/docs/src/Network-HTTP-Client-TLS.html#tlsManagerSettings
-- https://hackage.haskell.org/package/connection-0.2.6/docs/Network-Connection.html
tlsSettings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
