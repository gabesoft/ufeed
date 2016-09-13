-- | Feed specific values
module FeedConfig where

import Data.Maybe
import Text.HandsomeSoup
import Text.XML.HXT.Core

feedTransform :: ArrowXml a
              => String -> a XmlTree XmlTree
feedTransform url =
  fromMaybe this ((>>> root [] [this]) <$> lookup url feedProcessors)

inlineRequired :: String -> Bool
inlineRequired url =
  isJust (lookup url feedProcessors :: Maybe (IOLA XmlTree XmlTree))

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
