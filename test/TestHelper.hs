-- | Test helper functions
module TestHelper where

import Control.Exception (SomeException)
import Data.ByteString.Lazy as BS (readFile)
import Test.Hspec
import Text.XML
import Text.XML.Cursor
import Data.Either (rights)

readXml
  :: String -> IO (Either SomeException Document)
readXml file =
  do f <- BS.readFile file
     return $ parseLBS def f

extractAndVerifyFeed
  :: (Show a,Eq a)
  => (Document -> a) -> String -> a -> IO ()
extractAndVerifyFeed extractFeed file expected =
  do doc <- readXml file
     case doc of
       Left err -> fail (show err)
       Right d -> extractFeed d `shouldBe` expected

extractAndVerifyPosts
  :: (Show a,Eq a)
  => (Document -> [a]) -> String -> [a] -> IO ()
extractAndVerifyPosts extractPosts file expected =
  do doc <- readXml file
     case doc of
       Left e -> fail (show e)
       Right d -> extractPosts d `shouldBe` expected

docSample :: IO (Document,Cursor)
docSample =
  -- child c1 >>= element "{http://www.w3.org/2005/Atom}title" >>= descendant >>= content
  -- c1 $/ element "{http://www.w3.org/2005/Atom}title" &/ content
  -- attribute "href" c3
  -- attribute "href" <$> (c1 $/ element "{http://www.w3.org/2005/Atom}link")
  do d <- readXml "test/data/rss2.sample1.xml"
     let h = head $ rights [d]
     return (h,fromDocument h)