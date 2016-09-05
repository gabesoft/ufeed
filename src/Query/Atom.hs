-- |
-- Query atom feeds
module Query.Atom where

import Data.Maybe (listToMaybe, fromMaybe)
import Data.Text (Text, pack, unpack, strip)
import Model
import Text.XML
import Text.XML.Cursor
import Util

atomNS :: String
atomNS = "http://www.w3.org/2005/Atom"

toFeed :: Document -> Feed
toFeed doc =
  Feed {feedAuthor = get findAuthor
       ,feedDate = get findUpdated
       ,feedData = Nothing
       ,feedDescription = get findSubtitle
       ,feedFavicon = get findIcon
       ,feedGenerator = get findGenerator
       ,feedGuid = get findId
       ,feedId = Nothing
       ,feedImage = Image "" <$> get findLogo
       ,feedLanguage = Nothing
       ,feedLastPostDate = Nothing
       ,feedLastReadDate = Nothing
       ,feedLastReadStatus = Just ReadSuccess
       ,feedLink = link
       ,feedOriginalDescription = Nothing
       ,feedPostCount = 0
       ,feedTitle = fromMaybe "" $ get findTitle
       ,feedUri = ensureAbsoluteUrl link <$> uri}
  where cursor = fromDocument doc
        link = fromMaybe "" (get findAltLink)
        uri = get findSelfLink
        get f = unpack <$> f cursor

findAuthor :: Cursor -> Maybe Text
findAuthor cursor =
  maybeFirstText (cursor $/ axis "author" &/ axis "name" &/ content)

findUpdated :: Cursor -> Maybe Text
findUpdated = childContent "updated"

findTitle :: Cursor -> Maybe Text
findTitle = childContent "title"

findSubtitle :: Cursor -> Maybe Text
findSubtitle = childContent "subtitle"

findIcon :: Cursor -> Maybe Text
findIcon = childContent "icon"

findGenerator :: Cursor -> Maybe Text
findGenerator = childContent "generator"

findId :: Cursor -> Maybe Text
findId = childContent "id"

findLogo :: Cursor -> Maybe Text
findLogo = childContent "logo"

findSelfLink :: Cursor -> Maybe Text
findSelfLink = findLinkByRel (== Just (pack "self"))

findAltLink :: Cursor -> Maybe Text
findAltLink = findLinkByRel (/= Just (pack "self"))

findLinks
  :: Cursor -> [(Text,Maybe Text,Maybe Text)]
findLinks cursor = at <$> ls
  where ls = cursor $/ axis "link"
        at e =
          (Prelude.head $ attribute (nm "href") e
          ,listToMaybe $ attribute (nm "rel") e
          ,listToMaybe $ attribute (nm "type") e)

findLinkByRel
  :: (Maybe Text -> Bool) -> Cursor -> Maybe Text
findLinkByRel f cursor = listToMaybe (href <$> es)
  where es =
          Prelude.filter (f . rel)
                         (findLinks cursor)
        rel (_,r,_) = r
        href (h,_,_) = h

childContent :: String -> Cursor -> Maybe Text
childContent name cursor = maybeFirstText (cursor $/ axis name &/ content)

maybeFirstText :: [Text] -> Maybe Text
maybeFirstText = fmap strip . listToMaybe

-- |
-- Determine whether a document is an atom feed
isAtom :: Document -> Bool
isAtom doc = name == ns "feed"
  where root = documentRoot doc
        name = elementName root

-- |
-- Create a name-spaced name with the given local name
ns :: String -> Name
ns name =
  Name (pack name)
       (Just $ pack atomNS)
       Nothing

-- |
-- Create an non name-spaced name with the given local name
nm :: String -> Name
nm name = Name (pack name) Nothing Nothing

-- |
-- Create an axis from a local element name
axis :: String -> Axis
axis name = element (ns name)