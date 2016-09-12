-- | Html sanitizer
module Sanitizer (sanitize) where

import Data.Maybe
import FeedConfig
import Text.XML.HXT.Core

-- |
-- Perform some cleanup on an html text fragment. The cleanup includes
-- removing script and style tags, making all urls absolute, etc.
-- Both the feed url and the post base url are required parameters.
-- This function assumes html fragments as input but can handle full
-- html documents as well.
sanitize :: String -> String -> String -> String
sanitize feedUrl baseUrl =
  transformTree
    (feedTransform feedUrl >>>
     processTopDown removeEntities >>>
     processTopDown fixUrls >>> removeAllWhiteSpace)
  where fixUrls = toAbsoluteUrl baseUrl `when` (hasName "a" <+> hasName "img")

transformTree
  :: LA XmlTree XmlTree -> String -> String
transformTree arrows =
  concat .
  runLA (hread >>> selem "html" [this] >>> arrows >>> writeDocumentToString cfg)
  where cfg = [withOutputEncoding utf8,withOutputHTML,withRemoveWS yes]

removeByTag :: ArrowXml a
            => String -> a XmlTree XmlTree
removeByTag tag = filterA $ neg (hasName tag)

removeStyleLinks :: ArrowXml a
                 => a XmlTree XmlTree
removeStyleLinks =
  filterA $
  neg (isElem >>> hasName "link" >>> hasAttrValue "rel" (== "stylesheet"))

removeEmptyLinks :: ArrowXml a
                 => a XmlTree XmlTree
removeEmptyLinks = filterA $ neg (hasName "a" >>> neg getChildren)

removeEntities :: ArrowXml a
               => a XmlTree XmlTree
removeEntities =
  removeByTag "script" >>>
  removeByTag "style" >>>
  removeByTag "iframe" >>> removeStyleLinks >>> removeEmptyLinks

toAbsoluteUrl :: ArrowXml a
              => String -> a XmlTree XmlTree
toAbsoluteUrl base =
  processAttrl
    (mkAbsolute `when` (hasName "href" <+> hasName "src" <+> hasName "srcset"))
  where mkAbsolute =
          replaceChildren
            (xshow getChildren >>>
             arr (mkAbsoluteUrl base) `orElse` this >>> mkText)

mkAbsoluteUrl :: String -> String -> String
mkAbsoluteUrl base url = fromMaybe url (expandURIString url base)
