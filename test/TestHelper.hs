-- | Test helper functions
module TestHelper where

import           Control.Exception    (SomeException)
import           Data.ByteString.Lazy as BS (readFile)
import           Test.Hspec
import           Text.XML

readXml
  :: String -> IO (Either SomeException Document)
readXml file =
  do f <- BS.readFile file
     return $ parseLBS def f

extractAndVerifyFeed :: (Show a, Eq a) => (Document -> a) -> String -> a -> IO ()
extractAndVerifyFeed extractFeed file expected =
  do doc <- readXml file
     case doc of
       Left err -> fail (show err)
       Right d -> extractFeed d `shouldBe` expected

extractAndVerifyPosts
  :: (Show a, Eq a) => (Document -> [a]) -> String -> [a] -> IO ()
extractAndVerifyPosts extractPosts file expected =
  do doc <- readXml file
     case doc of
       Left e -> fail (show e)
       Right d -> extractPosts d `shouldBe` expected
