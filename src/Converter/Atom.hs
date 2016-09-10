-- |
-- Converter for atom 1.0 feeds
module Converter.Atom (extractFeed, extractPosts, isAtom) where

import Data.Maybe (listToMaybe, fromMaybe, fromJust)
import Data.Text (Text, pack, strip, empty)
import Types
import Text.XML
import Text.XML.Cursor

atomNS :: String
atomNS = "http://www.w3.org/2005/Atom"

extractFeed :: Document -> Feed
extractFeed doc =
  feed {feedAuthor = get findAuthor
       ,feedDate = get findUpdated
       ,feedDescription = get findSubtitle
       ,feedFavicon = get findIcon
       ,feedFormat = Just Atom1
       ,feedGenerator = get findGenerator
       ,feedGuid = get findId
       ,feedImage = Image empty <$> get findLogo
       ,feedLink = link
       ,feedPostCount = 0
       ,feedTitle = fromMaybe empty $ get findTitle}
  where cursor = fromDocument doc
        link = get findAltLink
        get f = f cursor
        feed = nullFeed empty

extractPosts :: Document -> [Post]
extractPosts doc = findPosts (fromDocument doc $/ axis "entry")

-- |
-- Determine whether a document is an atom feed
isAtom :: Document -> Bool
isAtom doc = name == ns "feed"
  where root = documentRoot doc
        name = elementName root

findPosts :: [Cursor] -> [Post]
findPosts = fmap findPost

findPost :: Cursor -> Post
findPost cursor =
  post {postAuthor = get findAuthor
       ,postDate = fromJust $ get findUpdated
       ,postDescription = get findContent
       ,postGuid = fromJust $ get findId
       ,postLink = fromJust link
       ,postPubdate = get findPublished
       ,postSummary = get findSummary
       ,postTitle = fromMaybe empty $ get findTitle}
  where get f = f cursor
        link = listToMaybe (href <$> findLinks cursor)
        href (h,_,_) = h
        post = nullPost

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