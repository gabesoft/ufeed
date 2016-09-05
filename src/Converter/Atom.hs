-- |
-- Converter for atom 1.0 feeds
module Converter.Atom (extractFeed,extractPosts,isAtom) where

import Data.Maybe (listToMaybe, fromMaybe, fromJust)
import Data.Text (Text, pack, strip, empty)
import Types
import Text.XML
import Text.XML.Cursor

atomNS :: String
atomNS = "http://www.w3.org/2005/Atom"

extractFeed :: Document -> Feed
extractFeed doc =
  Feed {feedAuthor = get findAuthor
       ,feedDate = get findUpdated
       ,feedData = Nothing
       ,feedDescription = get findSubtitle
       ,feedFavicon = get findIcon
       ,feedGenerator = get findGenerator
       ,feedGuid = get findId
       ,feedId = Nothing
       ,feedImage = Image empty <$> get findLogo
       ,feedLanguage = Nothing
       ,feedLastPostDate = Nothing
       ,feedLastReadDate = Nothing
       ,feedLastReadStatus = Just ReadSuccess
       ,feedLink = link
       ,feedOriginalDescription = Nothing
       ,feedPostCount = 0
       ,feedTitle = fromMaybe empty $ get findTitle
       ,feedUri = Nothing}
  where cursor = fromDocument doc
        link = fromMaybe empty (get findAltLink)
        get f = f cursor

extractPosts :: Document -> [Post]
extractPosts doc = findPosts (fromDocument doc $/ axis "entry")

findPosts :: [Cursor] -> [Post]
findPosts = fmap findPost

findPost :: Cursor -> Post
findPost cursor =
  Post {postAuthor = get findAuthor
       ,postComments = Nothing
       ,postDate = fromJust $ get findUpdated
       ,postDescription = get findContent
       ,postFeedId = Nothing
       ,postGuid = fromJust $ get findId
       ,postImage = Nothing
       ,postLink = fromJust link
       ,postPubdate = get findPublished
       ,postSummary = get findSummary
       ,postTitle = fromMaybe empty $ get findTitle}
  where get f = f cursor
        link = listToMaybe (href <$> findLinks cursor)
        href (h,_,_) = h

findContent :: Cursor -> Maybe Text
findContent = childContent "content"

findPublished :: Cursor -> Maybe Text
findPublished = childContent "published"

findSummary :: Cursor -> Maybe Text
findSummary = childContent "summary"

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
