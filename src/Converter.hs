-- | Extractor for feeds and posts
module Converter
  ( extractFeed
  , extractPosts
  , extractFeedAndPosts
  ) where

import Control.Exception
import qualified Converter.Atom as A
import qualified Converter.RSS as R
import Data.ByteString.Lazy
import Text.XML
import Types

data UnsupportedFormat =
  UnsupportedFormat
  deriving (Eq, Ord, Show)

instance Exception UnsupportedFormat

-- |
-- Extract feed meta-data and post entries from an XML byte-string
extractFeedAndPosts :: ByteString -> Either SomeException (Feed, [Post])
extractFeedAndPosts bytes = do
  doc <- parseLBS def bytes
  feed <- extractFeed' doc
  posts <- extractPosts' doc
  return (feed, posts)

-- |
-- Extract feed meta-data an XML byte-string
extractFeed :: ByteString -> Either SomeException Feed
extractFeed bytes = parseLBS def bytes >>= extractFeed'

-- |
-- Extract post entries from an XML byte-string
extractPosts :: ByteString -> Either SomeException [Post]
extractPosts bytes = parseLBS def bytes >>= extractPosts'

extractFeed' :: Document -> Either SomeException Feed
extractFeed' doc
  | A.isAtom doc = Right $ A.extractFeed doc
  | R.isRSS doc = Right $ R.extractFeed doc
  | otherwise = Left (toException UnsupportedFormat)

extractPosts' :: Document -> Either SomeException [Post]
extractPosts' doc
  | A.isAtom doc = Right $ A.extractPosts doc
  | R.isRSS doc = Right $ R.extractPosts doc
  | otherwise = Left (toException UnsupportedFormat)
