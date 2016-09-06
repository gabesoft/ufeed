-- | Converted between the different types of feed and post
module Converter (extractFeed, extractPosts, extractFeedAndPosts)
       where

import           Control.Exception
import qualified Converter.Atom       as A
import qualified Converter.RSS        as R
import           Data.ByteString.Lazy
import           Text.XML
import           Types

-- |
-- Extract feed meta-data and post entries from an XML byte-string
extractFeedAndPosts
  :: ByteString -> Either SomeException (Feed,[Post])
extractFeedAndPosts bytes =
  do doc <- parseLBS def bytes
     return (extractFeed' doc,extractPosts' doc)

-- |
-- Extract feed meta-data an XML byte-string
extractFeed
  :: ByteString -> Either SomeException Feed
extractFeed bytes = parseLBS def bytes >>= Right . extractFeed'

-- |
-- Extract post entries from an XML byte-string
extractPosts
  :: ByteString -> Either SomeException [Post]
extractPosts bytes = parseLBS def bytes >>= Right . extractPosts'

extractFeed' :: Document -> Feed
extractFeed' doc
  | A.isAtom doc = A.extractFeed doc
  | R.isRSS doc = R.extractFeed doc
  | otherwise = error "unsupported format"

extractPosts' :: Document -> [Post]
extractPosts' doc
  | A.isAtom doc = A.extractPosts doc
  | R.isRSS doc = R.extractPosts doc
  | otherwise = error "unsupported format"
