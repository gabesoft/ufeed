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

dcNS :: String
dcNS = "http://purl.org/dc/elements/1.1/"

contentNS :: String
contentNS = "http://purl.org/rss/1.0/modules/content/"

extractFeed :: Document -> Feed
extractFeed doc =
  feed {feedAuthor = get findEditor
       ,feedDate = get findUpdated
       ,feedDescription = get findDescription
       ,feedFormat = Just RSS2
       ,feedGenerator = get findGenerator
       ,feedGuid = get findLink
       ,feedImage = get findImage
       ,feedLanguage = get findLanguage
       ,feedLink = get findLink
       ,feedPostCount = 0
       ,feedTitle = fromJust $ get findTitle}
  where cursor = (fromDocument doc $/ axis "channel") & head
        get f = f cursor
        feed = nullFeed empty

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
  post {postAuthor = get findAuthor
       ,postDate = fromJust $ get findPubDate
       ,postDescription = get findContent <|> get findDescription
       ,postGuid = fromJust (guid <|> link)
       ,postLink = fromJust (link <|> guid)
       ,postPubdate = get findPubDate
       ,postSummary = get findDescription
       ,postTitle = fromMaybe empty $ get findTitle}
  where get f = f cursor
        link = get findLink
        guid = get findGuid -- TODO: if no guid is found create an md5 hash from title and description
        post = nullPost

findLanguage :: Cursor -> Maybe Text
findLanguage cursor =
  childContent "language" cursor <|> childContent "lang" cursor

findPubDate :: Cursor -> Maybe Text
findPubDate cursor =
  childContent "pubDate" cursor <|> childContent "date" cursor

findAuthor :: Cursor -> Maybe Text
findAuthor cursor =
  childContent "author" cursor <|> childContent "creator" cursor <|> dcCreator
  where dcCreator =
          childContentNS (ns dcNS "creator")
                         cursor

findEditor :: Cursor -> Maybe Text
findEditor cursor =
  childContent "managingEditor" cursor <|> childContent "creator" cursor

findUpdated :: Cursor -> Maybe Text
findUpdated = childContent "lastBuildDate"

findTitle :: Cursor -> Maybe Text
findTitle = childContent "title"

findDescription :: Cursor -> Maybe Text
findDescription = childContent "description"

findContent :: Cursor -> Maybe Text
findContent = childContentNS (ns contentNS "encoded")

findGenerator :: Cursor -> Maybe Text
findGenerator = childContent "generator"

findGuid :: Cursor -> Maybe Text
findGuid = childContent "guid"

findLink :: Cursor -> Maybe Text
findLink = childContent "link"

childContent :: String -> Cursor -> Maybe Text
childContent name cursor = maybeFirstText (cursor $/ axis name &/ content)

childContentNS :: Name -> Cursor -> Maybe Text
childContentNS name cursor = maybeFirstText (cursor $/ element name &/ content)

maybeFirstText :: [Text] -> Maybe Text
maybeFirstText = fmap strip . listToMaybe

-- |
-- Determine whether a document is an RSS feed
isRSS :: Document -> Bool
isRSS doc = name == nm "rss"
  where root = documentRoot doc
        name = elementName root

-- |
-- Create a name-spaced name with the given local name
ns :: String -> String -> Name
ns nspace name =
  Name (pack name)
       (Just $ pack nspace)
       Nothing

-- |
-- Create an non name-spaced name with the given local name
nm :: String -> Name
nm name = Name (pack name) Nothing Nothing

-- |
-- Create an axis from a local element name
axis :: String -> Axis
axis name = element (nm name)