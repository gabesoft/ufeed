-- | Html sanitizer
module Sanitizer
  ( sanitize
  , removeScriptTags
  , fixSrcset
  ) where

import Data.List (intercalate, isInfixOf)
import Data.Char (isSpace)
import Data.Maybe
import FeedConfig
import qualified Text.HTML.TagSoup.Tree as T
import Text.XML.HXT.Core

-- |
-- Perform some cleanup on an html text fragment. The cleanup includes
-- removing script and style tags, making all urls absolute, etc.
-- Both the feed url and the post base url are required parameters.
-- This function assumes html fragments as input but can handle full
-- html documents as well.
sanitize :: String -> String -> String -> String
sanitize feedUrl baseUrl = transformTree arrows . removeScriptTags
  where
    fixUrls = toAbsoluteUrl baseUrl `when` (hasName "a" <+> hasName "img")
    arrows =
      feedTransform feedUrl >>>
      processTopDown removeEntities >>> processTopDown fixUrls >>> removeAllWhiteSpace

transformTree :: LA XmlTree XmlTree -> String -> String
transformTree arrows =
  concat .
  runLA (hread >>> selem "html" [this] >>> arrows >>> writeDocumentToString cfg)
  where
    cfg = [withOutputEncoding utf8, withOutputHTML, withRemoveWS yes]

removeScriptTags :: String -> String
removeScriptTags html = T.renderTree $ T.transformTree script tree
  where
    tree = T.parseTree html
    script (T.TagBranch "script" _ _) = []
    script t = [t]

removeByTag
  :: ArrowXml a
  => String -> a XmlTree XmlTree
removeByTag tag = filterA $ neg (hasName tag)

removeStyleLinks
  :: ArrowXml a
  => a XmlTree XmlTree
removeStyleLinks =
  filterA $
  neg (isElem >>> hasName "link" >>> hasAttrValue "rel" (== "stylesheet"))

removeEmptyLinks
  :: ArrowXml a
  => a XmlTree XmlTree
removeEmptyLinks = filterA $ neg (hasName "a" >>> neg getChildren)

removeEntities
  :: ArrowXml a
  => a XmlTree XmlTree
removeEntities =
  removeByTag "script" >>>
  removeByTag "style" >>>
  removeByTag "iframe" >>> removeStyleLinks >>> removeEmptyLinks >>> removeShare

removeShare
  :: ArrowXml a
  => a XmlTree XmlTree
removeShare =
  filterA $
  neg
    (hasAttrValue "class" (isInfixOf "feedflare") <+>
     hasAttrValue "class" (isInfixOf "mediafed_ad") <+>
     hasAttrValue "id" (isInfixOf "jp-post-flair"))

toAbsoluteUrl
  :: ArrowXml a
  => String -> a XmlTree XmlTree
toAbsoluteUrl base =
  processAttrl
    (mkAbsolute mkAbsoluteUrl `when` (hasName "href" <+> hasName "src")) >>>
  processAttrl (mkAbsolute fixSrcset `when` hasName "srcset")
  where
    mkAbsolute f =
      replaceChildren
        (xshow getChildren >>> arr (f base) `orElse` this >>> mkText)

mkAbsoluteUrl :: String -> String -> String
mkAbsoluteUrl base url = fromMaybe url (expandURIString url base)

fixSrcset :: String -> String -> String
fixSrcset base =
  intercalate ", " . fmap (unwords . toAbs . split ' ') . split ','
  where
    toAbs :: [String] -> [String]
    toAbs [] = []
    toAbs [x] = [mkAbsoluteUrl base x]
    toAbs (x:xs) = mkAbsoluteUrl base x : xs
    split :: Char -> String -> [String]
    split c s =
      case dropWhile p s of
        "" -> []
        x ->
          let (w, r) = break (== c) x
          in w : split c r
      where
        p k = k == c || isSpace k
