-- | Converted between the different types of feed and post
module Converter where

import           Control.Exception
import           Converter.Atom
import           Converter.RSS
import           Data.ByteString
import           Types

extractFeed
  :: ByteString -> Either SomeException Feed
extractFeed = undefined-- extractFeed :: FT.Feed -> Feed
                       -- extractFeed (FT.AtomFeed f) = extractAtomFeed f
                       -- extractFeed (FT.RSSFeed f) = extractRSSFeed f
                       -- extractFeed (FT.RSS1Feed f) = extractRSS1Feed f
                       -- extractFeed (FT.XMLFeed _) = error "Could not parse feed"
                       -- extractPosts :: FT.Feed -> [Post]
                       -- extractPosts (FT.AtomFeed f) = extractAtomPosts f
                       -- extractRSS1Feed :: RSS1.Feed -> Feed
                       -- extractRSS1Feed = error "RSS1 is not implemented"
