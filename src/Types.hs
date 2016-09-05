{-# LANGUAGE RecordWildCards #-}

-- | Feed data types
module Types where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (pack, Text, empty)
import Data.Typeable
import Control.Exception

newtype UnsupportedFormat =
  UnsupportedFormat String
  deriving (Show,Typeable)

instance Exception UnsupportedFormat

type Date = Text

data Image =
  Image {imageTitle :: Text
        ,imageUrl :: Text}
  deriving (Eq,Show)

data LastModified =
  LastModified {etag :: Maybe Text
               ,lastModified :: Maybe Date}
  deriving (Eq,Show)

data ReadStatus
  = ReadSuccess
  | ReadFailure Text
  deriving (Eq,Show)

data Feed =
  Feed {feedAuthor :: Maybe Text
       ,feedData :: Maybe LastModified
       ,feedDate :: Maybe Date
       ,feedDescription :: Maybe Text
       ,feedFavicon :: Maybe Text
       ,feedGenerator :: Maybe Text
       ,feedGuid :: Maybe Text
       ,feedId :: Maybe Text
       ,feedImage :: Maybe Image
       ,feedLanguage :: Maybe Text
       ,feedLastPostDate :: Maybe Date
       ,feedLastReadDate :: Maybe Date
       ,feedLastReadStatus :: Maybe ReadStatus
       ,feedLink :: Text
       ,feedOriginalDescription :: Maybe Text
       ,feedPostCount :: Int
       ,feedTitle :: Text
       ,feedUri :: Maybe Text}
  deriving (Eq,Show)

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
       ,postTitle :: Text}
  deriving (Eq,Show)

nullLastModified :: LastModified
nullLastModified = LastModified Nothing Nothing

instance FromJSON Feed where
  parseJSON (Object o) =
    do feedAuthor <- o .:? pack "author"
       feedData <- o .:? pack "data"
       feedDate <- o .: pack "date"
       feedDescription <- o .:? pack "description"
       feedFavicon <- o .:? pack "favicon"
       feedGenerator <- o .:? pack "generator"
       feedGuid <- o .:? pack "guid"
       feedId <- o .:? pack "id"
       feedImage <- o .:? pack "image"
       feedLanguage <- o .:? pack "language"
       feedLastPostDate <- o .:? pack "lastPostDate"
       feedLastReadDate <- o .:? pack "lastReadDate"
       feedLastReadStatus <- o .:? pack "lastReadStatus"
       feedLink <- o .: pack "link"
       feedOriginalDescription <- o .:? pack "originalDescription"
       feedPostCount <- o .: pack "postCount"
       feedTitle <- o .: pack "title"
       feedUri <- o .: pack "uri"
       return Feed {..}
  parseJSON _ = fail "Expected an object for Feed"

instance FromJSON LastModified where
  parseJSON (Object v) =
    LastModified <$> v .:? pack "etag" <*> v .:? pack "last-modified"
  parseJSON _ = fail "Expected an object for LastModified"

instance FromJSON ReadStatus where
  parseJSON (Object v) =
    do readType <- v .: pack "readStatus"
       readError <- v .:? pack "readStatusError"
       return $
         case readType of
           "success" -> ReadSuccess
           _ -> ReadFailure (fromMaybe empty readError)
  parseJSON _ = fail "Expected an object for ReadStatus"

instance FromJSON Image where
  parseJSON =
    withObject "image" $
    \o ->
      do imageTitle <- o .:? pack "title"
         imageUrl <- o .: pack "url"
         return $ Image (fromMaybe empty imageTitle) imageUrl