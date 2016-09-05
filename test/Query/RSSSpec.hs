-- | Tests for Query.RSS
module Main (main) where

import Control.Exception (SomeException)
import Data.ByteString.Lazy as BS (readFile)
import Model
import Query.RSS
import Test.Hspec
import Text.XML as XML

files :: [String]
files =
  ["data/rss2.sample1.xml"
  ,"data/rss2.sample2.xml"
  ,"data/rss2.sample3.xml"
  ,"data/rss2.sample4.xml"]

cases :: [(String,Feed)]
cases = zip files feeds

readXml
  :: String -> IO (Either SomeException Document)
readXml file =
  do f <- BS.readFile file
     return $ parseLBS def f

main :: IO ()
main =
  hspec $
  describe "extract feed data" $
  mapM_ (\(f,e) -> it f $ extractAndVerify f e) cases

extractAndVerify :: String -> Feed -> IO ()
extractAndVerify file expected =
  do doc <- readXml file
     case doc of
       Left err -> fail (show err)
       Right d -> toFeed d `shouldBe` expected

feeds :: [Feed]
feeds =
  [Feed {feedAuthor = Just "editor@example.com"
        ,feedData = Nothing
        ,feedDate = Just "Tue, 10 Jun 2003 09:41:01 GMT"
        ,feedDescription = Just "Liftoff to Space Exploration."
        ,feedFavicon = Nothing
        ,feedGenerator = Just "Weblog Editor 2.0"
        ,feedGuid = Nothing
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Nothing
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = "http://liftoff.msfc.nasa.gov/"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "Liftoff News"
        ,feedUri = Nothing}
  ,Feed {feedAuthor = Nothing
        ,feedData = Nothing
        ,feedDate = Just "Fri, 26 Aug 2016 20:11:28 +0000"
        ,feedDescription = Just "Life at the bleeding edge (of web standards)"
        ,feedFavicon = Nothing
        ,feedGenerator = Just "https://wordpress.org/?v=4.6"
        ,feedGuid = Nothing
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Nothing
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = "http://lea.verou.me"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "Lea Verou"
        ,feedUri = Nothing}
  ,Feed {feedAuthor = Nothing
        ,feedData = Nothing
        ,feedDate = Nothing
        ,feedDescription = Just "Blog about npm things."
        ,feedFavicon = Nothing
        ,feedGenerator = Just "Tumblr (3.0; @npmjs)"
        ,feedGuid = Nothing
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Nothing
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = "http://blog.npmjs.org/"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "The npm Blog"
        ,feedUri = Nothing}
  ,Feed {feedAuthor = Nothing
        ,feedData = Nothing
        ,feedDate = Just "Mon, 18 Apr 2016 23:47:57 +0000"
        ,feedDescription =
           Just "Productivity hacks weekly in bite-size chunks. Just up your alley if you&#39;re <br/>at least slightly geeky or a tad bit obsessed with Evernote or WorkFlowy!"
        ,feedFavicon = Nothing
        ,feedGenerator =
           Just "Site-Server v6.0.0-8783-8783 (http://www.squarespace.com)"
        ,feedGuid = Nothing
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Nothing
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = "http://www.productivitymashup.com/"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "Productivity Mashup"
        ,feedUri = Nothing}]