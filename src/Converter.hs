-- | Converted between the different types of feed and post
module Converter where

import           Converter.Atom
import           Converter.RSS
import           Model
import qualified Text.Feed.Types as FT
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1
import Text.XML.Cursor

extractFeed :: FT.Feed -> Feed
extractFeed (FT.AtomFeed f) = extractAtomFeed f
extractFeed (FT.RSSFeed f) = extractRSSFeed f
extractFeed (FT.RSS1Feed f) = extractRSS1Feed f
extractFeed (FT.XMLFeed _) = error "Could not parse feed"

extractPosts :: FT.Feed -> [Post]
extractPosts (FT.AtomFeed f) = extractAtomPosts f

extractRSS1Feed :: RSS1.Feed -> Feed
extractRSS1Feed = error "RSS1 is not implemented"
