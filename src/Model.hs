{-# LANGUAGE RecordWildCards #-}

-- | Feed data models
module Model where

import           Data.Aeson
import           Data.Text  (Text, pack, unpack)

type Date = String

data Image =
  Image {imageTitle :: Maybe String
        ,imageUrl   :: String}
  deriving (Eq,Show)

data LastModified =
  LastModified {etag         :: Maybe String
               ,lastModified :: Maybe Date}
  deriving (Eq,Show)

data Feed =
  Feed {feedAuthor              :: Maybe String
       ,feedData                :: Maybe LastModified
       ,feedDate                :: Maybe Date
       ,feedDescription         :: Maybe String
       ,feedFavicon             :: Maybe String
       ,feedGenerator           :: Maybe String
       ,feedId                  :: String
       ,feedImage               :: Maybe Image
       ,feedLanguage            :: Maybe String
       ,feedLastPostDate        :: Maybe Date
       ,feedLink                :: String
       ,feedOriginalDescription :: Maybe String
       ,feedPostCount           :: Int
       ,feedTitle               :: String
       ,feedUri                 :: String}
  deriving (Eq,Show)

data Post =
  Post {postAuthor      :: String
       ,postComments    :: String
       ,postDate        :: Date
       ,postDescription :: String
       ,postFeedId      :: String
       ,postGuid        :: String
       ,postImage       :: Image
       ,postLink        :: String
       ,postPubdate     :: Date
       ,postSummary     :: String
       ,postTitle       :: String}
  deriving (Eq,Show)

instance FromJSON Feed where
  parseJSON (Object o) =
    do feedAuthor <- o .:? pack "author"
       feedData <- o .:? pack "data"
       feedDate <- o .:? pack "date"
       feedDescription <- o .:? pack "description"
       feedFavicon <- o .:? pack "favicon"
       feedGenerator <- o .:? pack "generator"
       feedId <- o .: pack "id"
       feedImage <- o .:? pack "image"
       feedLanguage <- o .:? pack "language"
       feedLastPostDate <- o .:? pack "lastPostDate"
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

instance FromJSON Image where
  parseJSON =
    withObject "image" $
    \o ->
      do imageTitle <- o .:? pack "title"
         imageUrl <- o .: pack "url"
         return Image {..}
