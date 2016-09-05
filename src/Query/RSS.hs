-- |
-- Query RSS feeds
module Query.RSS where

import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text (Text, pack, unpack, strip)
import Model
import Text.XML
import Text.XML.Cursor
import Data.Function ((&))

toFeed :: Document -> Feed
toFeed doc =
  Feed {feedAuthor = get findAuthor
       ,feedDate = get findUpdated
       ,feedData = Nothing
       ,feedDescription = get findDescription
       ,feedFavicon = Nothing
       ,feedGenerator = get findGenerator
       ,feedGuid = get findId
       ,feedId = Nothing
       ,feedImage = Image "" <$> get findLogo
       ,feedLanguage = Nothing
       ,feedLastPostDate = Nothing
       ,feedLastReadDate = Nothing
       ,feedLastReadStatus = Just ReadSuccess
       ,feedLink = fromMaybe "" (get findLink)
       ,feedOriginalDescription = Nothing
       ,feedPostCount = 0
       ,feedTitle = fromMaybe "" $ get findTitle
       ,feedUri = Nothing}
  where cursor = (fromDocument doc $/ axis "channel") & head
        get f = unpack <$> f cursor

findAuthor :: Cursor -> Maybe Text
findAuthor = childContent "managingEditor"

findUpdated :: Cursor -> Maybe Text
findUpdated = childContent "lastBuildDate"

findTitle :: Cursor -> Maybe Text
findTitle = childContent "title"

findDescription :: Cursor -> Maybe Text
findDescription = childContent "description"

findGenerator :: Cursor -> Maybe Text
findGenerator = childContent "generator"

findId :: Cursor -> Maybe Text
findId = childContent "id"

findLogo :: Cursor -> Maybe Text
findLogo = childContent "image"

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