-- | Test helper functions
module TestHelper where

import Control.Exception (SomeException)
import Data.ByteString.Lazy as BS (readFile)
import Data.Either (rights)
import Test.Hspec
import Text.XML
import Text.XML.Cursor

readXml :: String -> IO (Either SomeException Document)
readXml file = do
  f <- BS.readFile file
  return $ parseLBS def f

extractAndVerify
  :: (Show a, Eq a)
  => (Document -> a) -> String -> a -> IO ()
extractAndVerify extract file expected =
  readXml file >>= either (fail . show) (\d -> extract d `shouldBe` expected)

docSample :: IO (Document, Cursor)
docSample = do
  d <- readXml "test/data/rss2.sample1.xml"
  let h = head $ rights [d]
  return (h, fromDocument h)
