-- | Html sanitizer
module Sanitizer where

import Data.Maybe
-- import Text.HandsomeSoup
import Text.XML.HXT.Core

-- expandURIString "index.html" "http://localhost:8000"
-- apply (deep $ isElem >>> (hasName "a" <+> hasName "link")
--                      >>> getAttrValue "href"
--                      >>> arr (\s -> "[" ++ s ++ "]") >>> mkText) pg1
-- xshow converts an xmltree into a string
-- TODO: set the base uri with (setDefaultBaseURI base) -- must be in IO
-- get the nodes rather than strings
-- runLA (hread >>> (css "a" <+> css "link") >>> getAttrValue "href") pg1
-- runLA (hread >>> (css "link" <+> css "a") >>> (getElemName &&& getAttrValue "href")) pg1
-- runLA (hread >>> css "script" >>> getChildren >>> getText) pg1
-- pg1 <- readFile "test/data/pg.swan.html"
apply :: LA XmlTree XmlTree -> String -> String
apply arrows = head . runLA (hread >>> arrows >>> writeDocumentToString cfg)
  where cfg = [withOutputEncoding utf8, withOutputHTML]

applyId :: String -> String
applyId = apply this

-- |
-- Perform some cleanup on an html text fragment. The cleanup includes
-- removing script and style tags, making all urls absolute, etc
sanitize :: String -> String -> String
sanitize baseUrl =
  apply (processTopDown removeEntities >>>
         processTopDown (toAbsoluteUrl baseUrl) >>> removeAllWhiteSpace)

-- where isHref = isElem >>> hasName "a" >>> hasAttr "href"
--       isImg = isElem >>> hasName "img"
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

-- TODO: remove
sanitizeOld :: String -> IO String
sanitizeOld html = head <$> output
  where rcfg = [withParseHTML yes,withSubstDTDEntities yes]
        wcfg = [withIndent yes,withOutputEncoding utf8]
        output = runX $ readString rcfg html >>> writeDocumentToString wcfg

-- example of making abs urls for images
-- maybe use expandURIString to avoid being in IO
mkAbsImageRef :: IOStateArrow s XmlTree XmlTree
mkAbsImageRef = processAttrl (mkAbsRef `when` hasName "src")
  where mkAbsRef =
          replaceChildren
            (xshow getChildren >>> (mkAbsURI `orElse` this) >>> mkText)