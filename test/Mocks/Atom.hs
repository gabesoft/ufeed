{-# LANGUAGE OverloadedStrings #-}

-- | Mock for atom tests
module Mocks.Atom where

import Types

feedCases :: [(String,Feed)]
feedCases = zip files feeds

postsCases :: [(String,[Post])]
postsCases = zip (take 2 files) posts

files :: [String]
files =
  ["test/data/atom1.0.sample0.xml"
  ,"test/data/atom1.0.sample1.xml"
  ,"test/data/atom1.0.sample2.xml"
  ,"test/data/atom1.0.sample3.xml"
  ,"test/data/atom1.0.sample4.xml"]

feeds :: [Feed]
feeds =
  [Feed {feedAuthor = Just "John Doe"
        ,feedData = Nothing
        ,feedDate = Just "2003-12-13T18:30:02Z"
        ,feedDescription = Nothing
        ,feedFavicon = Nothing
        ,feedFormat = Just Atom1
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
        ,feedFormat = Just Atom1
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
        ,feedUri = Nothing}
  ,Feed {feedAuthor = Just "Bozhidar Batsov"
        ,feedData = Nothing
        ,feedDate = Just "2015-06-17T12:45:14+03:00"
        ,feedDescription = Nothing
        ,feedFavicon = Nothing
        ,feedFormat = Just Atom1
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
        ,feedUri = Nothing}
  ,Feed {feedAuthor = Nothing
        ,feedData = Nothing
        ,feedDate = Just "2014-11-17T00:00:00+02:00"
        ,feedDescription = Nothing
        ,feedFavicon = Nothing
        ,feedFormat = Just Atom1
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
        ,feedUri = Nothing}
  ,Feed {feedAuthor = Nothing
        ,feedData = Nothing
        ,feedDate = Just "2016-09-05T00:13:20Z"
        ,feedDescription = Just "most recent 30 from stackoverflow.com"
        ,feedFavicon = Nothing
        ,feedFormat = Just Atom1
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
        ,feedUri = Nothing}]

posts :: [[Post]]
posts =
  [[Post {postAuthor = Nothing
         ,postComments = Nothing
         ,postDate = "2003-12-13T18:30:02Z"
         ,postDescription = Nothing
         ,postFeedId = Nothing
         ,postGuid = "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"
         ,postImage = Nothing
         ,postLink = "http://example.org/2003/12/13/atom03"
         ,postPubdate = Nothing
         ,postSummary = Just "Some text."
         ,postTitle = "Atom-Powered Robots Run Amok"}]
  ,[Post {postAuthor = Just "Mark Pilgrim"
         ,postComments = Nothing
         ,postDate = "2005-07-31T12:29:29Z"
         ,postDescription = Just ""
         ,postFeedId = Nothing
         ,postGuid = "tag:example.org,2003:3.2397"
         ,postImage = Nothing
         ,postLink = "http://example.org/2005/04/02/atom"
         ,postPubdate = Just "2003-12-13T08:29:29-04:00"
         ,postSummary = Nothing
         ,postTitle = "Atom draft-07 snapshot"}]]