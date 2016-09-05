-- | Tests for Query.Atom
module Main (main) where

import Control.Exception (SomeException)
import Data.ByteString.Lazy as BS (readFile)
import Model
import Query.Atom
import Test.Hspec
import Text.XML as XML

files :: [String]
files =
  ["data/atom0.3.sample1.xml"
  ,"data/atom1.0.sample0.xml"
  ,"data/atom1.0.sample1.xml"
  ,"data/atom1.0.sample2.xml"
  ,"data/atom1.0.sample3.xml"
  ,"data/atom1.0.sample4.xml"]

cases :: [(String,Feed)]
cases = zip (tail files) feeds

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
  [Feed {feedAuthor = Just "John Doe"
        ,feedData = Nothing
        ,feedDate = Just "2003-12-13T18:30:02Z"
        ,feedDescription = Nothing
        ,feedFavicon = Nothing
        ,feedGenerator = Nothing
        ,feedGuid = Just "urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6"
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Nothing
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = "http://example.org/"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "Example Feed"
        ,feedUri = Nothing}
  ,Feed {feedAuthor = Nothing
        ,feedData = Nothing
        ,feedDate = Just "2005-07-31T12:29:29Z"
        ,feedDescription =
           Just "A <em>lot</em> of effort\n    went into making this effortless"
        ,feedFavicon = Nothing
        ,feedGenerator = Just "Example Toolkit"
        ,feedGuid = Just "tag:example.org,2003:3"
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Nothing
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = "http://example.org/"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "dive into mark"
        ,feedUri = Just "http://example.org/feed.atom"}
  ,Feed {feedAuthor = Just "Bozhidar Batsov"
        ,feedData = Nothing
        ,feedDate = Just "2015-06-17T12:45:14+03:00"
        ,feedDescription = Nothing
        ,feedFavicon = Nothing
        ,feedGenerator = Just "Octopress"
        ,feedGuid = Just "http://batsov.com/"
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Nothing
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = "http://batsov.com/"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "(think)"
        ,feedUri = Just "http://batsov.com/atom.xml"}
  ,Feed {feedAuthor = Nothing
        ,feedData = Nothing
        ,feedDate = Just "2014-11-17T00:00:00+02:00"
        ,feedDescription = Nothing
        ,feedFavicon = Nothing
        ,feedGenerator = Nothing
        ,feedGuid = Just "urn:emacsrocks-com:feed"
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Nothing
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = ""
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "Emacs Rocks!"
        ,feedUri = Just "http://emacsrocks.com/atom.xml"}
  ,Feed {feedAuthor = Nothing
        ,feedData = Nothing
        ,feedDate = Just "2016-09-05T00:13:20Z"
        ,feedDescription = Just "most recent 30 from stackoverflow.com"
        ,feedFavicon = Nothing
        ,feedGenerator = Nothing
        ,feedGuid =
           Just "http://stackoverflow.com/feeds/tag?tagnames=haskell&sort=newest"
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Nothing
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink =
           "http://stackoverflow.com/questions/tagged/?tagnames=haskell&sort=newest"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "Newest questions tagged haskell - Stack Overflow"
        ,feedUri =
           Just "http://stackoverflow.com/feeds/tag?tagnames=haskell&sort=newest"}]