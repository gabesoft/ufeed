{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Feed data types
module Types where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe (fromMaybe)
import Data.Text (Text, empty)

type Date = Text

data FeedFormat
  = RSS2
  | Atom1
  deriving (Eq, Show)

data Image = Image
  { imageTitle :: Text
  , imageUrl :: Text
  } deriving (Eq, Show)

data LastModified = LastModified
  { etag :: Maybe Text
  , lastModified :: Maybe Date
  } deriving (Eq, Show)

nullLastModified :: LastModified
nullLastModified = LastModified Nothing Nothing

data ReadStatus
  = ReadSuccess
  | ReadFailure Text
  deriving (Eq, Show)

data FeedSubscription = FeedSubscription
  { subscriptionId :: Text
  , subscriptionFeedId :: Text
  } deriving (Eq, Show)

data Feed = Feed
  { feedAuthor :: Maybe Text
  , feedDate :: Maybe Date
  , feedDescription :: Maybe Text
  , feedFailedAttempts :: Int
  , feedFavicon :: Maybe Text
  , feedFormat :: Maybe FeedFormat
  , feedGenerator :: Maybe Text
  , feedGuid :: Maybe Text
  , feedId :: Maybe Text
  , feedImage :: Maybe Image
  , feedLanguage :: Maybe Text
  , feedLastModified :: Maybe LastModified
  , feedLastPostDate :: Maybe Date
  , feedLastReadDate :: Maybe Date
  , feedLastReadStatus :: Maybe ReadStatus
  , feedLink :: Maybe Text
  , feedPostCount :: Int
  , feedTitle :: Text
  , feedUri :: Text
  } deriving (Eq, Show)

nullFeed :: Text -> Feed
nullFeed uri =
  Feed
  { feedAuthor = Nothing
  , feedLastModified = Nothing
  , feedDate = Nothing
  , feedDescription = Nothing
  , feedFailedAttempts = 0
  , feedFavicon = Nothing
  , feedFormat = Nothing
  , feedGenerator = Nothing
  , feedGuid = Nothing
  , feedId = Nothing
  , feedImage = Nothing
  , feedLanguage = Nothing
  , feedLastPostDate = Nothing
  , feedLastReadDate = Nothing
  , feedLastReadStatus = Nothing
  , feedLink = Nothing
  , feedPostCount = 0
  , feedTitle = empty
  , feedUri = uri
  }

data Post = Post
  { postAuthor :: Maybe Text
  , postComments :: Maybe Text
  , postDate :: Maybe Date
  , postDescription :: Maybe Text
  , postFeedId :: Maybe Text
  , postGuid :: Text
  , postImage :: Maybe Image
  , postId :: Maybe Text
  , postInlineStatus :: Maybe ReadStatus
  , postLink :: Text
  , postPubdate :: Maybe Date
  , postSummary :: Maybe Text
  , postTitle :: Maybe Text
  } deriving (Eq, Show)

nullPost :: Post
nullPost =
  Post
  { postAuthor = Nothing
  , postComments = Nothing
  , postDate = Nothing
  , postDescription = Nothing
  , postFeedId = Nothing
  , postGuid = empty
  , postId = Nothing
  , postImage = Nothing
  , postInlineStatus = Nothing
  , postLink = empty
  , postPubdate = Nothing
  , postSummary = Nothing
  , postTitle = Nothing
  }

data UserPost = UserPost
  { userPostSubscriptionId :: Text
  , userPostRead :: Bool
  , userPostPostId :: Text
  } deriving (Eq, Show)

instance FromJSON FeedSubscription where
  parseJSON (Object o) = FeedSubscription <$> o .: "_id" <*> o .: "feedId"
  parseJSON _ = fail "Expected an object for FeedSubscription"

instance FromJSON Feed where
  parseJSON (Object o) = do
    feedAuthor <- o .:? "author"
    feedLastModified <- o .:? "data"
    feedDate <- o .:? "date"
    feedDescription <- o .:? "description"
    feedFailedAttempts <- fromMaybe 0 <$> (o .:? "failedAttempts")
    feedFavicon <- o .:? "favicon"
    feedFormat <- o .:? "format"
    feedGenerator <- o .:? "generator"
    feedGuid <- o .:? "guid"
    feedId <- (o .:? "_id" <|> o .:? "id")
    feedImage <- o .:? "image"
    feedLanguage <- o .:? "language"
    feedLastPostDate <- o .:? "lastPostDate"
    feedLastReadDate <- o .:? "lastReadDate"
    feedLastReadStatus <- o .:? "lastReadStatus"
    feedLink <- o .: "link"
    feedPostCount <- o .: "postCount"
    feedTitle <- o .: "title"
    feedUri <- o .: "uri"
    return Feed {..}
  parseJSON _ = fail "Expected an object for Feed"

instance ToJSON Feed where
  toJSON Feed {..} =
    omitNulls
      [ "author" .= feedAuthor
      , "data" .= feedLastModified
      , "date" .= feedDate
      , "description" .= feedDescription
      , "failedAttempts" .= feedFailedAttempts
      , "favicon" .= feedFavicon
      , "format" .= feedFormat
      , "generator" .= feedGenerator
      , "guid" .= feedGuid
      , "_id" .= feedId
      , "image" .= feedImage
      , "language" .= feedLanguage
      , "lastPostDate" .= feedLastPostDate
      , "lastReadDate" .= feedLastReadDate
      , "lastReadStatus" .= feedLastReadStatus
      , "link" .= feedLink
      , "postCount" .= feedPostCount
      , "title" .= feedTitle
      , "uri" .= feedUri
      ]

instance FromJSON Post where
  parseJSON (Object o) = do
    postAuthor <- o .:? "author"
    postComments <- o .:? "comments"
    postDate <- o .:? "date"
    postDescription <- o .:? "description"
    postFeedId <- o .:? "feedId"
    postGuid <- fromMaybe empty <$> (o .:? "guid")
    postId <- (o .:? "_id" <|> o .:? "id")
    postImage <- o .:? "image"
    postInlineStatus <- o .:? "inlineStatus"
    postLink <- fromMaybe empty <$> (o .:? "link")
    postPubdate <- o .:? "pubdate"
    postSummary <- o .:? "summary"
    postTitle <- o .:? "title"
    return Post {..}
  parseJSON _ = fail "Expected an object for Post"

instance ToJSON Post where
  toJSON Post {..} =
    omitNulls
      [ "author" .= postAuthor
      , "comments" .= postComments
      , "date" .= postDate
      , "description" .= postDescription
      , "feedId" .= postFeedId
      , "guid" .= postGuid
      , "_id" .= postId
      , "image" .= postImage
      , "inlineStatus" .= postInlineStatus
      , "link" .= postLink
      , "pubdate" .= postPubdate
      , "summary" .= postSummary
      , "title" .= postTitle
      ]

instance ToJSON UserPost where
  toJSON o =
    object
      [ "subscriptionId" .= userPostSubscriptionId o
      , "postId" .= userPostPostId o
      , "read" .= userPostRead o
      ]

instance FromJSON FeedFormat where
  parseJSON (String v) = format v
    where
      format :: Text -> Parser FeedFormat
      format f =
        case f of
          "rss-2.0" -> return RSS2
          "atom-1.0" -> return Atom1
          _ -> fail "unknown format"
  parseJSON _ = fail "Expected an object of FeedFormat"

instance ToJSON FeedFormat where
  toJSON o = String (format o)
    where
      format :: FeedFormat -> Text
      format RSS2 = "rss-2.0"
      format Atom1 = "atom-1.0"

instance FromJSON LastModified where
  parseJSON (Object v) = LastModified <$> v .:? "etag" <*> v .:? "last-modified"
  parseJSON _ = fail "Expected an object for LastModified"

instance ToJSON LastModified where
  toJSON o = object ["etag" .= etag o, "last-modified" .= lastModified o]

instance FromJSON ReadStatus where
  parseJSON (Object v) = do
    readType <- v .: "readStatus"
    readError <- v .:? "readStatusError"
    return (status readType readError)
    where
      status :: String -> Maybe Text -> ReadStatus
      status typ err =
        case typ of
          "success" -> ReadSuccess
          _ -> ReadFailure (fromMaybe empty err)
  parseJSON _ = fail "Expected an object for ReadStatus"

instance ToJSON ReadStatus where
  toJSON o = object ["readStatus" .= status o, "readStatusError" .= serror o]
    where
      status :: ReadStatus -> Text
      status ReadSuccess = "success"
      status (ReadFailure _) = "failure"
      serror :: ReadStatus -> Maybe Text
      serror (ReadFailure f) = Just f
      serror _ = Nothing

instance FromJSON Image where
  parseJSON =
    withObject "image" $ \o -> do
      imageTitle <- o .:? "title"
      imageUrl <- o .: "url"
      return $ Image (fromMaybe empty imageTitle) imageUrl

instance ToJSON Image where
  toJSON o = object ["title" .= imageTitle o, "url" .= imageUrl o]

omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull
  where
    notNull (_, Null) = False
    notNull _ = True
