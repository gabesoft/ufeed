-- | Html sanitizer
module Sanitizer (sanitize) where

import Data.Maybe
import Text.HandsomeSoup
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
    (feedSpecific feedUrl >>>
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

feedSpecific :: ArrowXml a
             => String -> a XmlTree XmlTree
feedSpecific feedUrl =
  fromMaybe this ((>>> root [] [this]) <$> lookup feedUrl feedProcessors)

feedProcessors :: ArrowXml a
               => [(String,a XmlTree XmlTree)]
feedProcessors =
  [("http://alvinalexander.com/taxonomy/term/4787/0/feed",css "#content")
  ,("http://andrew.gibiansky.com/feed.rss",css "article .entry-content")
  ,("http://blog.aaronbieber.com/feed.xml",css ".entry-content")
  ,("http://demosthenes.info/feed.php",css ".entry-content")
  ,("http://feeds.feedburner.com/TheGeekStuff",css ".post_content")
  ,("http://feeds.feedburner.com/bocoup",css ".blog-content")
  ,("http://feeds.feedburner.com/creativebloq/.net",css "#article-body")
  ,("http://techieme.in/feed/",css ".single_post")
  ,("http://underscore.io/blog/feed.xml",css "article.blog-post-content")
  ,("http://www.47deg.com/feed",css ".detail-post")
  ,("http://www.aaronsw.com/2002/feeds/pgessays.rss",css "table table font")
  ,("http://www.howardism.org/index.xml",css "#content")
  ,("http://www.iandevlin.com/blog/feed",css "main")
  ,("https://bartoszmilewski.com/feed/",css ".post-content")
  ,("https://benfrain.com/feed",css "article")
  ,("https://blog.openshift.com/feed/",css "main")
  ,("https://m.signalvnoise.com/feed",css "article main")
  ,("https://medium.com/feed/@abhinavchhikara",css "article main")
  ,("https://medium.com/feed/@azerbike",css "article main")
  ,("https://medium.com/feed/@bantic",css "article main")
  ,("https://medium.com/feed/@dustin",css "article main")
  ,("https://medium.com/feed/@jasonlong",css "article main")
  ,("https://medium.com/feed/@joshblack",css "article main")
  ,("https://medium.com/feed/@jtpaasch",css "article main")
  ,("https://medium.com/feed/@kevindeasis",css "article main")
  ,("https://medium.com/feed/@markoxvee",css "article main")
  ,("https://medium.com/feed/@tjholowaychuk",css "article main")
  ,("https://medium.freecodecamp.com/feed",css "article main")
  ,("https://strongloop.com/feed/",css ".entry-content")]