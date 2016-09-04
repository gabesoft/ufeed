{-# LANGUAGE RecordWildCards #-}

-- | Feed data models
module Model where

import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (pack)

type Date = String

data Image =
  Image {imageTitle :: String
        ,imageUrl :: String}
  deriving (Eq,Show)

data LastModified =
  LastModified {etag :: Maybe String
               ,lastModified :: Maybe Date}
  deriving (Eq,Show)

data ReadStatus
  = ReadSuccess
  | ReadFailure String
  deriving (Eq,Show)

data Feed =
  Feed {feedAuthor :: Maybe String
       ,feedData :: Maybe LastModified
       ,feedDate :: Maybe Date
       ,feedDescription :: Maybe String
       ,feedFavicon :: Maybe String
       ,feedGenerator :: Maybe String
       ,feedGuid :: Maybe String
       ,feedId :: Maybe String
       ,feedImage :: Maybe Image
       ,feedLanguage :: Maybe String
       ,feedLastPostDate :: Maybe Date
       ,feedLastReadDate :: Maybe Date
       ,feedLastReadStatus :: Maybe ReadStatus
       ,feedLink :: String
       ,feedOriginalDescription :: Maybe String
       ,feedPostCount :: Int
       ,feedTitle :: String
       ,feedUri :: Maybe String}
  deriving (Eq,Show)

data Post =
  Post {postAuthor :: Maybe String
       ,postComments :: Maybe String
       ,postDate :: Date
       ,postDescription :: Maybe String
       ,postFeedId :: Maybe String
       ,postGuid :: String
       ,postImage :: Maybe Image
       ,postLink :: String
       ,postPubdate :: Maybe Date
       ,postSummary :: Maybe String
       ,postTitle :: String}
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
           _ -> ReadFailure (fromMaybe "" readError)
  parseJSON _ = fail "Expected an object for ReadStatus"

instance FromJSON Image where
  parseJSON =
    withObject "image" $
    \o ->
      do imageTitle <- o .:? pack "title"
         imageUrl <- o .: pack "url"
         return $ Image (fromMaybe "" imageTitle) imageUrl
