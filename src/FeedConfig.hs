-- | Feed specific values
module FeedConfig where

import Data.Maybe
import Text.HandsomeSoup
import Text.XML.HXT.Core

feedTransform
  :: ArrowXml a
  => String -> a XmlTree XmlTree
feedTransform url =
  fromMaybe this ((>>> root [] [this]) <$> lookup url feedProcessors)

inlineRequired :: String -> Bool
inlineRequired url =
  isJust (lookup url feedProcessors :: Maybe (IOLA XmlTree XmlTree))

feedProcessors
  :: ArrowXml a
  => [(String, a XmlTree XmlTree)]
feedProcessors =
  [ ("http://alvinalexander.com/taxonomy/term/4787/0/feed", css "#content")
  , ("http://andrew.gibiansky.com/feed.rss", css "article .entry-content")
  , ("http://blog.aaronbieber.com/feed.xml", css ".entry-content")
  , ("http://demosthenes.info/feed.php", css ".entry-content")
  , ("http://feeds.feedburner.com/TheGeekStuff", css ".post_content")
  , ("http://feeds.feedburner.com/bocoup", css ".blog-content")
  , ("http://feeds.feedburner.com/creativebloq/.net", css "#article-body")
  , ("http://feeds.feedburner.com/philipwalton", css ".entry-content")
  , ("http://sample.for.test/feed", css ".entry-content")
  , ("http://techieme.in/feed/", css "main article")
  , ("http://thenewcode.com/feed.php", css ".entry-content")
  , ("http://una.im/feed.xml", css "article")
  , ("http://underscore.io/blog/feed.xml", css "article.blog-post-content")
  , ("http://www.47deg.com/feed", css ".detail-post")
  , ("http://www.aaronsw.com/2002/feeds/pgessays.rss", css "table table font")
  , ("http://www.howardism.org/index.xml", css "#content")
  , ("http://www.iandevlin.com/blog/feed", css "main")
  , ("http://www.martinfowler.com/feed.atom", css ".paperBody")
  , ("https://bartoszmilewski.com/feed/", css ".post-content")
  , ("https://benfrain.com/feed", css "article")
  , ("https://blog.openshift.com/feed/", css "main")
  , ("https://m.signalvnoise.com/feed", css mediumSelector)
  , ("https://medium.com/feed/@MrJamesFisher", css mediumSelector)
  , ("https://medium.com/feed/@abhinavchhikara", css mediumSelector)
  , ("https://medium.com/feed/@azerbike", css mediumSelector)
  , ("https://medium.com/feed/@bantic", css mediumSelector)
  , ("https://medium.com/feed/@dustin", css mediumSelector)
  , ("https://medium.com/feed/@jasonlong", css mediumSelector)
  , ("https://medium.com/feed/@chetcorcos", css mediumSelector)
  , ("https://medium.com/feed/@joshblack", css mediumSelector)
  , ("https://medium.com/feed/@jtpaasch", css mediumSelector)
  , ("https://medium.com/feed/@kevindeasis", css mediumSelector)
  , ("https://medium.com/feed/@markoxvee", css mediumSelector)
  , ("https://medium.com/feed/@tjholowaychuk", css mediumSelector)
  , ("https://medium.freecodecamp.com/feed", css mediumSelector)
  , ("https://strongloop.com/feed/", css ".entry-content")
  , ("https://webresourcesdepot.com/feed/", css "article.post")
  , ("https://zenhack.net/rss.xml", css "article")
  ]

mediumSelector :: String
mediumSelector = ".postArticle-content"
