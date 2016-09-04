-- | Convert atom feeds and posts
module Converter.Atom where

import Data.Maybe (isNothing)
import Model
import qualified Text.Atom.Feed as A
import Util

extractAtomFeed :: A.Feed -> Feed
extractAtomFeed feed =
  Feed {feedAuthor = (extractAuthor . A.feedAuthors) feed
       ,feedData = Nothing
       ,feedDate = Just $ A.feedUpdated feed
       ,feedDescription = Nothing
       ,feedFavicon = A.feedIcon feed
       ,feedGenerator = A.genText <$> A.feedGenerator feed
       ,feedGuid = Just $ A.feedId feed
       ,feedId = Nothing
       ,feedImage = Image "" <$> A.feedLogo feed
       ,feedLanguage = Nothing
       ,feedLastPostDate = Nothing
       ,feedLastReadDate = Nothing
       ,feedLastReadStatus = Just ReadSuccess
       ,feedLink = link
       ,feedOriginalDescription = Nothing
       ,feedPostCount = 0
       ,feedTitle = extractFeedTitle feed
       ,feedUri = Just uri}
  where (link,uri) = extractLinks feed

extractLinks :: A.Feed -> (String,String)
extractLinks feed = (link,ensureAbsoluteUrl link uri)
  where filterByRel f =
          filter (f . A.linkRel)
                 (A.feedLinks feed)
        self = filterByRel ((== "self") . rel)
        other = filterByRel ((== "alternate") . rel) ++ filterByRel isNothing
        rel Nothing = ""
        rel (Just (Right ncName)) = ncName
        rel (Just (Left url)) = url
        uri = A.linkHref . head $ self
        link =
          case other of
            [] -> A.feedId feed
            x:_ -> A.linkHref x

extractFeedTitle :: A.Feed -> String
extractFeedTitle feed = title (A.feedTitle feed)
  where title (A.TextString s) = s
        title (A.HTMLString s) = s
        title _ = A.feedId feed

extractAuthor :: [A.Person] -> Maybe String
extractAuthor [] = Nothing
extractAuthor (x:_) = Just $ A.personName x

extractAtomPosts :: A.Feed -> [Post]
extractAtomPosts feed = extractPost feed <$> A.feedEntries feed

extractPost :: A.Feed -> A.Entry -> Post
extractPost feed entry =
  Post {postAuthor = extractAuthor (A.entryAuthors entry)
       ,postComments = Nothing
       ,postDate = A.entryUpdated entry
       ,postDescription = extractEntryContent <$> A.entryContent entry
       ,postFeedId = Nothing
       ,postGuid = A.entryId entry
       ,postImage = Nothing
       ,postLink = extractEntryLink feed entry
       ,postPubdate = A.entryPublished entry
       ,postSummary = extractTextContent <$> A.entrySummary entry
       ,postTitle = extractTextContent (A.entryTitle entry)}

extractEntryLink :: A.Feed -> A.Entry -> String
extractEntryLink feed entry =
  case links of
    [] -> ensureAbsoluteUrl fLink $ A.entryId entry
    x:_ -> ensureAbsoluteUrl fLink $ A.linkHref x
  where links = A.entryLinks entry
        (fLink,_) = extractLinks feed

extractEntryContent :: A.EntryContent -> String
extractEntryContent ec =
  case ec of
    A.TextContent c -> c
    A.HTMLContent c -> c
    _ -> ""

extractTextContent :: A.TextContent -> String
extractTextContent tc =
  case tc of
    A.TextString s -> s
    A.HTMLString s -> s
    _ -> ""
