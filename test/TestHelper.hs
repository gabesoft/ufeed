{-# LANGUAGE OverloadedStrings #-}

-- | Test helper functions
module TestHelper where

import qualified Api
import Control.Exception (SomeException)
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
  [ "https://www.reddit.com/r/vim/.rss"                                            -- 0
  , "https://www.reddit.com/r/programming/.rss"                                    -- 1
  , "https://www.reddit.com/r/emacs/.rss"                                          -- 2
  , "http://alexyoung.org/feed.rss"                                                -- 3
  , "http://demosthenes.info/feed.php"                                             -- 4
  , "http://antirez.com/rss"                                                       -- 5
  , "http://simplyaccessible.com/?feed=rss2"                                       -- 6
  , "http://simplyaccessible.com/feed/"                                            -- 7
  , "http://dev.af83.com/atom.xml"                                                 -- 8
  , "http://feeds.feedblitz.com/sethsblog&x=1"                                     -- 9
  , "http://engineering.wingify.com/atom.xml"                                      -- 10
  , "https://www.youtube.com/feeds/videos.xml?channel_id=UC8BtBl8PNgd3vWKtm2yJ7aA" -- 11
  , "http://www.youtube.com/feeds/videos.xml?channel_id=UC8BtBl8PNgd3vWKtm2yJ7aA"  -- 12
  , "http://www.nathantypanski.com/atom.xml"                                       -- 13
  , "http://www.yesodweb.com/feed"                                                 -- 14
  , "http://degoes.net/feed.xml"                                                   -- 15 (empty link causes a mongo error)
  , "http://feeds.feedburner.com/Fsharpforfunandprofit?format=xml"                 -- 16 (utf error)
  , "http://feeds.feedburner.com/LinkedInBlog"                                     -- 17
  ]

sampleHost :: String
sampleHost = "http://localhost:8006"

kapiHost :: String
kapiHost = "http://localhost:8001"

runUpdate :: Text -> IO (Either SomeException (Feed, [Post]))
runUpdate url = do
  maybeFeed <- Api.fetchFeedByUri kapiHost url
  case maybeFeed of
    Left e -> return (Left e)
    Right feed -> update (envForUpdate kapiHost) feed
