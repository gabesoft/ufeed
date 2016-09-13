{-# LANGUAGE OverloadedStrings #-}

-- | Mock for atom tests
module Mocks.Atom where

import Types
import Data.Text (empty)

feedCases :: [(String, Feed)]
feedCases = zip files feeds

postsCases :: [(String, [Post])]
postsCases = zip (take 2 files) posts

files :: [String]
files =
  [ "test/data/atom1.0.sample0.xml"
  , "test/data/atom1.0.sample1.xml"
  , "test/data/atom1.0.sample2.xml"
  , "test/data/atom1.0.sample3.xml"
  , "test/data/atom1.0.sample4.xml"
  ]

feeds :: [Feed]
feeds =
  [ (nullFeed empty)
    { feedAuthor = Just "John Doe"
    , feedDate = Just "2003-12-13T18:30:02Z"
    , feedDescription = Nothing
    , feedFormat = Just Atom1
    , feedGenerator = Nothing
    , feedGuid = Just "urn:uuid:60a76c80-d399-11d9-b93C-0003939e0af6"
    , feedLink = Just "http://example.org/"
    , feedTitle = "Example Feed"
    }
  , (nullFeed empty)
    { feedAuthor = Nothing
    , feedDate = Just "2005-07-31T12:29:29Z"
    , feedDescription =
      Just "A <em>lot</em> of effort\n    went into making this effortless"
    , feedFormat = Just Atom1
    , feedGenerator = Just "Example Toolkit"
    , feedGuid = Just "tag:example.org,2003:3"
    , feedLink = Just "http://example.org/"
    , feedTitle = "dive into mark"
    }
  , (nullFeed empty)
    { feedAuthor = Just "Bozhidar Batsov"
    , feedDate = Just "2015-06-17T12:45:14+03:00"
    , feedDescription = Nothing
    , feedFormat = Just Atom1
    , feedGenerator = Just "Octopress"
    , feedGuid = Just "http://batsov.com/"
    , feedLink = Just "http://batsov.com/"
    , feedTitle = "(think)"
    }
  , (nullFeed empty)
    { feedAuthor = Nothing
    , feedDate = Just "2014-11-17T00:00:00+02:00"
    , feedDescription = Nothing
    , feedFormat = Just Atom1
    , feedGenerator = Nothing
    , feedGuid = Just "urn:emacsrocks-com:feed"
    , feedLink = Nothing
    , feedTitle = "Emacs Rocks!"
    }
  , (nullFeed empty)
    { feedAuthor = Nothing
    , feedDate = Just "2016-09-05T00:13:20Z"
    , feedDescription = Just "most recent 30 from stackoverflow.com"
    , feedFormat = Just Atom1
    , feedGenerator = Nothing
    , feedGuid =
      Just "http://stackoverflow.com/feeds/tag?tagnames=haskell&sort=newest"
    , feedLink =
      Just
        "http://stackoverflow.com/questions/tagged/?tagnames=haskell&sort=newest"
    , feedTitle = "Newest questions tagged haskell - Stack Overflow"
    }
  ]

posts :: [[Post]]
posts =
  [ [ nullPost
      { postAuthor = Nothing
      , postDate = "2003-12-13T18:30:02Z"
      , postDescription = Nothing
      , postGuid = "urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a"
      , postLink = "http://example.org/2003/12/13/atom03"
      , postPubdate = Nothing
      , postSummary = Just "Some text."
      , postTitle = Just "Atom-Powered Robots Run Amok"
      }
    ]
  , [ nullPost
      { postAuthor = Just "Mark Pilgrim"
      , postDate = "2005-07-31T12:29:29Z"
      , postDescription = Just ""
      , postGuid = "tag:example.org,2003:3.2397"
      , postLink = "http://example.org/2005/04/02/atom"
      , postPubdate = Just "2003-12-13T08:29:29-04:00"
      , postSummary = Nothing
      , postTitle = Just "Atom draft-07 snapshot"
      }
    ]
  ]
