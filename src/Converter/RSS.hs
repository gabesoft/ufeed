-- |
-- Converter for RSS 2.0 feeds
module Converter.RSS (extractFeed, extractPosts, isRSS) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Function ((&))
import Data.Maybe (listToMaybe, fromMaybe, fromJust)
import Data.Text (Text, pack, strip, empty)
import Text.XML
import Text.XML.Cursor
import Types

extractFeed :: Document -> Feed
extractFeed doc =
  Feed {feedAuthor = get findEditor
       ,feedDate = get findUpdated
       ,feedData = Nothing
       ,feedDescription = get findDescription
       ,feedFavicon = Nothing
       ,feedGenerator = get findGenerator
       ,feedGuid = get findLink
       ,feedId = Nothing
       ,feedImage = get findImage
       ,feedLanguage = get findLanguage
       ,feedLastPostDate = Nothing
       ,feedLastReadDate = Nothing
       ,feedLastReadStatus = Just ReadSuccess
       ,feedLink = fromJust $ get findLink
       ,feedOriginalDescription = Nothing
       ,feedPostCount = 0
       ,feedTitle = fromJust $ get findTitle
       ,feedUri = Nothing}
  where cursor = (fromDocument doc $/ axis "channel") & head
        get f = f cursor

findImage :: Cursor -> Maybe Image
findImage cursor = join $ listToMaybe (img <$> cs)
  where cs = cursor $/ axis "image"
        img c = Image <$> childContent "title" c <*> childContent "url" c

extractPosts :: Document -> [Post]
extractPosts doc =
  findPosts (fromDocument doc $/ axis "channel" &/ axis "item")

findPosts :: [Cursor] -> [Post]
findPosts = fmap findPost

findPost :: Cursor -> Post
findPost cursor =
  Post {postAuthor = get findAuthor
       ,postComments = Nothing
       ,postDate = fromJust $ get findPubDate
       ,postDescription = get findDescription
       ,postFeedId = Nothing
       ,postGuid = fromJust (guid <|> link)
       ,postImage = Nothing
       ,postLink = fromJust (link <|> guid)
       ,postPubdate = get findPubDate
       ,postSummary = Nothing
       ,postTitle = fromMaybe empty $ get findTitle}
  where get f = f cursor
        link = get findLink
        guid = get findGuid -- TODO: if no guid is found create an md5 hash from title and description

findLanguage :: Cursor -> Maybe Text
findLanguage cursor =
  childContent "language" cursor <|> childContent "lang" cursor

findPubDate :: Cursor -> Maybe Text
findPubDate cursor =
  childContent "pubDate" cursor <|> childContent "date" cursor

findAuthor :: Cursor -> Maybe Text
findAuthor cursor =
  childContent "author" cursor <|> childContent "creator" cursor

findEditor :: Cursor -> Maybe Text
findEditor cursor =
  childContent "managingEditor" cursor <|> childContent "creator" cursor

findUpdated :: Cursor -> Maybe Text
findUpdated = childContent "lastBuildDate"

findTitle :: Cursor -> Maybe Text
findTitle = childContent "title"

findDescription :: Cursor -> Maybe Text
findDescription = childContent "description"

findGenerator :: Cursor -> Maybe Text
findGenerator = childContent "generator"

findGuid :: Cursor -> Maybe Text
findGuid = childContent "guid"

findLink :: Cursor -> Maybe Text
findLink = childContent "link"

childContent :: String -> Cursor -> Maybe Text
childContent name cursor = maybeFirstText (cursor $/ axis name &/ content)

maybeFirstText :: [Text] -> Maybe Text
maybeFirstText = fmap strip . listToMaybe

-- |
-- Determine whether a document is an RSS feed
isRSS :: Document -> Bool
isRSS doc = name == nm "rss"
  where root = documentRoot doc
        name = elementName root

-- |
-- Create an non name-spaced name with the given local name
nm :: String -> Name
nm name = Name (pack name) Nothing Nothing

-- |
-- Create an axis from a local element name
axis :: String -> Axis
axis name = element (nm name)