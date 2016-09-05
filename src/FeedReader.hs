-- | Functions for fetching feeds data
module FeedReader where

import Control.Exception (SomeException)
import Control.Lens ((&), (.~), (^.), (^?))
import Data.ByteString.Char8 as C (pack)
import Data.ByteString.Lazy as BS (readFile)
import Data.ByteString.Lazy.Char8 as LC (unpack)
import Data.Either (rights)
import Data.Maybe
import Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8)
import Types
import Network.HTTP.Types.Header
import Network.Wreq
import Text.XML as XML
import Text.XML.Cursor
import Data.ByteString (ByteString)

sample :: IO (Document,Cursor)
sample =
  -- child c1 >>= element "{http://www.w3.org/2005/Atom}title" >>= descendant >>= content
  -- c1 $/ element "{http://www.w3.org/2005/Atom}title" &/ content
  -- attribute "href" c3
  -- attribute "href" <$> (c1 $/ element "{http://www.w3.org/2005/Atom}link")
  do d <- readXml "test/data/rss2.sample1.xml"
     let h = head $ rights [d]
     return (h,fromDocument h)

readXml
  :: String -> IO (Either SomeException Document)
readXml file =
  do f <- BS.readFile file
     return $ parseLBS def f

-- TODO return Either SomeException ByteString
-- |
-- Fetch a feed and all its posts that are newer than
-- the specified last modified information
fetchFeed
  :: String
  -> LastModified
  -> IO (Either SomeException (ByteString,LastModified))
fetchFeed uri modified = undefined

-- do res <- getWith (modifiedHeaders defaults modified) uri
--    let feed = parseFeedString . LC.unpack . (^. responseBody) $ res
--    let et = res ^? responseHeader hETag
--    let lm = res ^? responseHeader hLastModified
--    return (feed
--           ,LastModified (decodeUtf8 <$> et)
--                         (decodeUtf8 <$> lm))
-- |
-- Add the last modified headers to the default options
modifiedHeaders
  :: Options -> LastModified -> Options
modifiedHeaders opts modified = foldr step opts (fs <*> [modified])
  where fs = [(,) hIfNoneMatch . etag,(,) hIfModifiedSince . lastModified]
        step (h,v) d =
          d & header h .~ [(C.pack . fromMaybe "") (T.unpack <$> v)]