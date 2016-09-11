{-# LANGUAGE RecordWildCards #-}

-- | Feed data types
module Types where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (pack, Text, empty)

type Date = Text

data FeedFormat
  = RSS2
  | Atom1
  deriving (Eq,Show)

data Image =
  Image {imageTitle :: Text
        ,imageUrl :: Text}
  deriving (Eq,Show)

data LastModified =
  LastModified {etag :: Maybe Text
               ,lastModified :: Maybe Date}
  deriving (Eq,Show)

nullLastModified :: LastModified
nullLastModified = LastModified Nothing Nothing

data ReadStatus
  = ReadSuccess
  | ReadFailure Text
  deriving (Eq,Show)

data Feed =
  Feed {feedAuthor :: Maybe Text
       ,feedDate :: Maybe Date
       ,feedDescription :: Maybe Text
       ,feedFavicon :: Maybe Text
       ,feedFormat :: Maybe FeedFormat
       ,feedGenerator :: Maybe Text
       ,feedGuid :: Maybe Text
       ,feedId :: Maybe Text
       ,feedImage :: Maybe Image
       ,feedLanguage :: Maybe Text
       ,feedLastModified :: Maybe LastModified
       ,feedLastPostDate :: Maybe Date
       ,feedLastReadDate :: Maybe Date
       ,feedLastReadStatus :: Maybe ReadStatus
       ,feedLink :: Maybe Text
       ,feedPostCount :: Int
       ,feedTitle :: Text
       ,feedUri :: Text}
  deriving (Eq,Show)

nullFeed :: Text -> Feed
nullFeed uri =
  Feed {feedAuthor = Nothing
       ,feedLastModified = Nothing
       ,feedDate = Nothing
       ,feedDescription = Nothing
       ,feedFavicon = Nothing
       ,feedFormat = Nothing
       ,feedGenerator = Nothing
       ,feedGuid = Nothing
       ,feedId = Nothing
       ,feedImage = Nothing
       ,feedLanguage = Nothing
       ,feedLastPostDate = Nothing
       ,feedLastReadDate = Nothing
       ,feedLastReadStatus = Nothing
       ,feedLink = Nothing
       ,feedPostCount = 0
       ,feedTitle = empty
       ,feedUri = uri}

data Post =
  Post {postAuthor :: Maybe Text
       ,postComments :: Maybe Text
       ,postDate :: Date
       ,postDescription :: Maybe Text
       ,postFeedId :: Maybe Text
       ,postGuid :: Text
       ,postImage :: Maybe Image
       ,postLink :: Text
       ,postPubdate :: Maybe Date
       ,postSummary :: Maybe Text
       ,postTitle :: Maybe Text}
  deriving (Eq,Show)

nullPost :: Post
nullPost =
  Post {postAuthor = Nothing
       ,postComments = Nothing
       ,postDate = empty
       ,postDescription = Nothing
       ,postFeedId = Nothing
       ,postGuid = empty
       ,postImage = Nothing
       ,postLink = empty
       ,postPubdate = Nothing
       ,postSummary = Nothing
       ,postTitle = Nothing}

instance FromJSON Feed where
  parseJSON (Object o) =
    do feedAuthor <- o .:? pack "author"
       feedLastModified <- o .:? pack "data"
       feedDate <- o .: pack "date"
       feedDescription <- o .:? pack "description"
       feedFavicon <- o .:? pack "favicon"
       feedFormat <- o .:? pack "feedFormat"
       feedGenerator <- o .:? pack "generator"
       feedGuid <- o .:? pack "guid"
       feedId <- o .:? pack "id"
       feedImage <- o .:? pack "image"
       feedLanguage <- o .:? pack "language"
       feedLastPostDate <- o .:? pack "lastPostDate"
       feedLastReadDate <- o .:? pack "lastReadDate"
       feedLastReadStatus <- o .:? pack "lastReadStatus"
       feedLink <- o .: pack "link"
       feedPostCount <- o .: pack "postCount"
       feedTitle <- o .: pack "title"
       feedUri <- o .: pack "uri"
       return Feed {..}
  parseJSON _ = fail "Expected an object for Feed"

instance FromJSON Post where
  parseJSON (Object o) =
    do postAuthor <- o .:? pack "author"
       postComments <- o .:? pack "comments"
       postDate <- o .: pack "date"
       postDescription <- o .:? pack "description"
       postFeedId <- o .:? pack "feedid"
       postGuid <- o .: pack "guid"
       postImage <- o .:? pack "image"
       postLink <- o .: pack "link"
       postPubdate <- o .:? pack "pubdate"
       postSummary <- o .:? pack "summary"
       postTitle <- o .:? pack "title"
       return Post {..}
  parseJSON _ = fail "Expected an object for Post"

instance FromJSON FeedFormat where
  parseJSON (Object v) =
    do ftype <- v .: pack "format"
       format ftype
    where format :: String -> Parser FeedFormat
          format f =
            case f of
              "rss-2.0" -> return RSS2
              "atom-1.0" -> return Atom1
              _ -> fail "unknown format"
  parseJSON _ = fail "Expected an object of FeedFormat"

instance FromJSON LastModified where
  parseJSON (Object v) =
    LastModified <$> v .:? pack "etag" <*> v .:? pack "last-modified"
  parseJSON _ = fail "Expected an object for LastModified"

instance FromJSON ReadStatus where
  parseJSON (Object v) =
    do readType <- v .: pack "readStatus"
       readError <- v .:? pack "readStatusError"
       return (status readType readError)
    where status
            :: String -> Maybe Text -> ReadStatus
          status typ err =
            case typ of
              "success" -> ReadSuccess
              _ -> ReadFailure (fromMaybe empty err)
  parseJSON _ = fail "Expected an object for ReadStatus"

instance FromJSON Image where
  parseJSON =
    withObject "image" $
    \o ->
      do imageTitle <- o .:? pack "title"
         imageUrl <- o .: pack "url"
         return $ Image (fromMaybe empty imageTitle) imageUrl
