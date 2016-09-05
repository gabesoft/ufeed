{-# LANGUAGE OverloadedStrings #-}

-- | Mock data for RSS tests
module Mocks.RSS where

import Types

feedCases :: [(String,Feed)]
feedCases = zip files feeds

postsCases :: [(String,[Post])]
postsCases = zip (take 2 files) posts

files :: [String]
files =
  ["test/data/rss2.sample1.xml"
  ,"test/data/rss2.sample2.xml"
  ,"test/data/rss2.sample3.xml"
  ,"test/data/rss2.sample4.xml"]

feeds :: [Feed]
feeds =
  [Feed {feedAuthor = Just "editor@example.com"
        ,feedData = Nothing
        ,feedDate = Just "Tue, 10 Jun 2003 09:41:01 GMT"
        ,feedDescription = Just "Liftoff to Space Exploration."
        ,feedFavicon = Nothing
        ,feedGenerator = Just "Weblog Editor 2.0"
        ,feedGuid = Just "http://liftoff.msfc.nasa.gov/"
        ,feedId = Nothing
        ,feedImage =
           Just Image {imageTitle = "logo"
                      ,imageUrl = "http://placeholder.it/20/30"}
        ,feedLanguage = Just "en-us"
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = "http://liftoff.msfc.nasa.gov/"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "Liftoff News"
        ,feedUri = Nothing}
  ,Feed {feedAuthor = Nothing
        ,feedData = Nothing
        ,feedDate = Just "Fri, 26 Aug 2016 20:11:28 +0000"
        ,feedDescription = Just "Life at the bleeding edge (of web standards)"
        ,feedFavicon = Nothing
        ,feedGenerator = Just "https://wordpress.org/?v=4.6"
        ,feedGuid = Just "http://lea.verou.me"
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Just "en-US"
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = "http://lea.verou.me"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "Lea Verou"
        ,feedUri = Nothing}
  ,Feed {feedAuthor = Nothing
        ,feedData = Nothing
        ,feedDate = Nothing
        ,feedDescription = Just "Blog about npm things."
        ,feedFavicon = Nothing
        ,feedGenerator = Just "Tumblr (3.0; @npmjs)"
        ,feedGuid = Just "http://blog.npmjs.org/"
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Nothing
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = "http://blog.npmjs.org/"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "The npm Blog"
        ,feedUri = Nothing}
  ,Feed {feedAuthor = Nothing
        ,feedData = Nothing
        ,feedDate = Just "Mon, 18 Apr 2016 23:47:57 +0000"
        ,feedDescription =
           Just "Productivity hacks weekly in bite-size chunks. Just up your alley if you&#39;re <br/>at least slightly geeky or a tad bit obsessed with Evernote or WorkFlowy!"
        ,feedFavicon = Nothing
        ,feedGenerator =
           Just "Site-Server v6.0.0-8783-8783 (http://www.squarespace.com)"
        ,feedGuid = Just "http://www.productivitymashup.com/"
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Just "en-US"
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Just ReadSuccess
        ,feedLink = "http://www.productivitymashup.com/"
        ,feedOriginalDescription = Nothing
        ,feedPostCount = 0
        ,feedTitle = "Productivity Mashup"
        ,feedUri = Nothing}]

posts :: [[Post]]
posts =
  [[Post {postAuthor = Nothing
         ,postComments = Nothing
         ,postDate = "Tue, 03 Jun 2003 09:39:21 GMT"
         ,postDescription =
            Just "How do Americans get ready to work with Russians aboard the International Space Station? They take a crash course in culture, language and protocol at Russia's <a href=\"http://howe.iki.rssi.ru/GCTC/gctc_e.htm\">Star City</a>."
         ,postFeedId = Nothing
         ,postGuid = "http://liftoff.msfc.nasa.gov/2003/06/03.html#item573"
         ,postImage = Nothing
         ,postLink = "http://liftoff.msfc.nasa.gov/news/2003/news-starcity.asp"
         ,postPubdate = Just "Tue, 03 Jun 2003 09:39:21 GMT"
         ,postSummary = Nothing
         ,postTitle = "Star City"}
   ,Post {postAuthor = Nothing
         ,postComments = Nothing
         ,postDate = "Fri, 30 May 2003 11:06:42 GMT"
         ,postDescription =
            Just "Sky watchers in Europe, Asia, and parts of Alaska and Canada will experience a <a href=\"http://science.nasa.gov/headlines/y2003/30may_solareclipse.htm\">partial eclipse of the Sun</a> on Saturday, May 31st."
         ,postFeedId = Nothing
         ,postGuid = "http://liftoff.msfc.nasa.gov/2003/05/30.html#item572"
         ,postImage = Nothing
         ,postLink = "http://liftoff.msfc.nasa.gov/2003/05/30.html#item572"
         ,postPubdate = Just "Fri, 30 May 2003 11:06:42 GMT"
         ,postSummary = Nothing
         ,postTitle = ""}
   ,Post {postAuthor = Nothing
         ,postComments = Nothing
         ,postDate = "Tue, 27 May 2003 08:37:32 GMT"
         ,postDescription =
            Just "Before man travels to Mars, NASA hopes to design new engines that will let us fly through the Solar System more quickly.  The proposed VASIMR engine would do that."
         ,postFeedId = Nothing
         ,postGuid = "http://liftoff.msfc.nasa.gov/2003/05/27.html#item571"
         ,postImage = Nothing
         ,postLink = "http://liftoff.msfc.nasa.gov/news/2003/news-VASIMR.asp"
         ,postPubdate = Just "Tue, 27 May 2003 08:37:32 GMT"
         ,postSummary = Nothing
         ,postTitle = "The Engine That Does More"}
   ,Post {postAuthor = Nothing
         ,postComments = Nothing
         ,postDate = "Tue, 20 May 2003 08:56:02 GMT"
         ,postDescription =
            Just "Compared to earlier spacecraft, the International Space Station has many luxuries, but laundry facilities are not one of them.  Instead, astronauts have other options."
         ,postFeedId = Nothing
         ,postGuid = "http://liftoff.msfc.nasa.gov/2003/05/20.html#item570"
         ,postImage = Nothing
         ,postLink = "http://liftoff.msfc.nasa.gov/news/2003/news-laundry.asp"
         ,postPubdate = Just "Tue, 20 May 2003 08:56:02 GMT"
         ,postSummary = Nothing
         ,postTitle = "Astronauts' Dirty Laundry"}]
  ,[Post {postAuthor = Nothing
         ,postComments = Nothing
         ,postDate = "Fri, 26 Aug 2016 20:07:58 +0000"
         ,postDescription =
            Just "I have often lamented how many\160JavaScript developers don&#8217;t realize that\160a large percentage of\160HTML &#38; CSS authors are not comfortable writing JS, and struggle to use their libraries. To encourage libraries with HTML APIs, i.e. libraries that can be used without writing a line of JS, I made a website to list and promote them: markapp.io. [&#8230;]"
         ,postFeedId = Nothing
         ,postGuid = "http://lea.verou.me/?p=2649"
         ,postImage = Nothing
         ,postLink = "http://feedproxy.google.com/~r/leaverou/~3/7ilYG6MJKUI/"
         ,postPubdate = Just "Fri, 26 Aug 2016 20:07:58 +0000"
         ,postSummary = Nothing
         ,postTitle = "Markapp: A list of HTML libraries"}
   ,Post {postAuthor = Nothing
         ,postComments = Nothing
         ,postDate = "Tue, 31 May 2016 22:13:00 +0000"
         ,postDescription =
            Just "As part of my preparation for my talk at CSSDay HTML Special, I was perusing\160the most recent HTML specs (WHATWG Living Standard, W3C HTML 5.1) to see what undiscovered gems lay there. It turns out that HTML sliders have a lot of cool features specced that aren&#8217;t very well implemented: Ticks that snap via the [&#8230;]"
         ,postFeedId = Nothing
         ,postGuid = "http://lea.verou.me/?p=2621"
         ,postImage = Nothing
         ,postLink = "http://feedproxy.google.com/~r/leaverou/~3/CgEmFMNXTjI/"
         ,postPubdate = Just "Tue, 31 May 2016 22:13:00 +0000"
         ,postSummary = Nothing
         ,postTitle =
            "Introducing Multirange: A tiny polyfill for HTML5.1 two-handle sliders"}
   ,Post {postAuthor = Nothing
         ,postComments = Nothing
         ,postDate = "Thu, 17 Dec 2015 02:55:28 +0000"
         ,postDescription =
            Just "Women speaking up about the sexism they have experienced in tech is great\160for raising awareness about the issues. However, when no positive stories get out, the overall picture painted is bleak, which could scare even more women away. Lucky for me, I fell in love with programming a decade\160before I even heard there is a [&#8230;]"
         ,postFeedId = Nothing
         ,postGuid = "http://lea.verou.me/?p=2590"
         ,postImage = Nothing
         ,postLink = "http://feedproxy.google.com/~r/leaverou/~3/h62mIYhVziE/"
         ,postPubdate = Just "Thu, 17 Dec 2015 02:55:28 +0000"
         ,postSummary = Nothing
         ,postTitle = "My positive experience as a woman in tech"}]]