{-# LANGUAGE OverloadedStrings #-}

-- | Test helper functions
module TestHelper where

import qualified Api
import Control.Exception (SomeException)
import Control.Lens
import Data.ByteString.Lazy as BS (readFile)
import Data.Either (rights)
import Data.Text (Text)
import FeedUpdater
import Test.Hspec
import Text.XML
import Text.XML.Cursor
import Types

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

sampleFeeds :: [Text]
sampleFeeds =
  [ "https://www.reddit.com/r/vim/.rss" -- "566e6d7213415194b8df8008"
  , "https://www.reddit.com/r/programming/.rss" -- "566e72068e864928ba4f291c"
  , "https://www.reddit.com/r/emacs/.rss" -- "566e6d8b13415194b8df8024"
  , "http://alexyoung.org/feed.rss"
  ]

sampleHost :: String
sampleHost = "http://localhost:8006"

runUpdate :: Text -> IO (Either SomeException (Feed, [Post]))
runUpdate url = do
  maybeFeed <- Api.fetchFeedByUri sampleHost url
  case maybeFeed of
    Left e -> return (Left e)
    Right feed -> update (envForUpdate sampleHost) feed
