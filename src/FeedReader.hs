-- | Functions for fetching feeds data
module FeedReader where

import Control.Exception (SomeException)
import Control.Lens ((^.), (.~), (^?), (&))
import Data.ByteString.Char8 as C (pack, unpack)
import Data.ByteString.Lazy.Char8 as LC (pack, unpack)
import Data.ByteString.Lazy as BS (readFile)
import Data.Either (rights)
import Data.Maybe
import Model
import Network.HTTP.Types.Header
import Network.Wreq
import Text.Feed.Import (parseFeedString)
import qualified Text.Feed.Types as FT
import Text.XML as XML
import Text.XML.Cursor

sample :: IO (Document,Cursor)
sample =
  -- child c1 >>= element "{http://www.w3.org/2005/Atom}title" >>= descendant >>= content
  -- c1 $/ element "{http://www.w3.org/2005/Atom}title" &/ content
  -- attribute "href" c3
  -- attribute "href" <$> (c1 $/ element "{http://www.w3.org/2005/Atom}link")
  do d <- readXml "data/atom1.0.sample1.xml"
     let h = head $ rights [d]
     return (h,fromDocument h)

readXml
  :: String -> IO (Either SomeException Document)
readXml file = do
  f <- BS.readFile file
  return $ parseLBS def f

fetchFeed
  :: String -> LastModified -> IO (Maybe FT.Feed,LastModified)
fetchFeed uri modified =
  do res <- getWith (modifiedHeaders modified) uri
     let feed = parseFeedString . LC.unpack . (^. responseBody) $ res
     let et = res ^? responseHeader hETag
     let lm = res ^? responseHeader hLastModified
     return (feed
            ,LastModified (C.unpack <$> et)
                          (C.unpack <$> lm))

modifiedHeaders :: LastModified -> Options
modifiedHeaders modified = foldr step defaults (fs <*> [modified])
  where fs = [(,) hIfNoneMatch . etag,(,) hIfModifiedSince . lastModified]
        step (h,v) d = d & header h .~ [(C.pack . fromMaybe "") v]