{-# LANGUAGE OverloadedStrings #-}

-- | Mock data for RSS tests
module Mocks.RSS where

import Types
import Data.Text (empty)

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
        ,feedLastModified = Nothing
        ,feedDate = Just "Tue, 10 Jun 2003 09:41:01 GMT"
        ,feedDescription = Just "Liftoff to Space Exploration."
        ,feedFavicon = Nothing
        ,feedFormat = Just RSS2
        ,feedGenerator = Just "Weblog Editor 2.0"
        ,feedGuid = Just "http://liftoff.msfc.nasa.gov/"
        ,feedId = Nothing
        ,feedImage =
           Just Image {imageTitle = "logo"
                      ,imageUrl = "http://placeholder.it/20/30"}
        ,feedLanguage = Just "en-us"
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Nothing
        ,feedLink = Just "http://liftoff.msfc.nasa.gov/"
        ,feedPostCount = 0
        ,feedTitle = "Liftoff News"
        ,feedUri = empty}
  ,Feed {feedAuthor = Nothing
        ,feedLastModified = Nothing
        ,feedDate = Just "Fri, 26 Aug 2016 20:11:28 +0000"
        ,feedDescription = Just "Life at the bleeding edge (of web standards)"
        ,feedFavicon = Nothing
        ,feedFormat = Just RSS2
        ,feedGenerator = Just "https://wordpress.org/?v=4.6"
        ,feedGuid = Just "http://lea.verou.me"
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Just "en-US"
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Nothing
        ,feedLink = Just "http://lea.verou.me"
        ,feedPostCount = 0
        ,feedTitle = "Lea Verou"
        ,feedUri = empty}
  ,Feed {feedAuthor = Nothing
        ,feedLastModified = Nothing
        ,feedDate = Nothing
        ,feedDescription = Just "Blog about npm things."
        ,feedFavicon = Nothing
        ,feedFormat = Just RSS2
        ,feedGenerator = Just "Tumblr (3.0; @npmjs)"
        ,feedGuid = Just "http://blog.npmjs.org/"
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Nothing
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Nothing
        ,feedLink = Just "http://blog.npmjs.org/"
        ,feedPostCount = 0
        ,feedTitle = "The npm Blog"
        ,feedUri = empty}
  ,Feed {feedAuthor = Nothing
        ,feedLastModified = Nothing
        ,feedDate = Just "Mon, 18 Apr 2016 23:47:57 +0000"
        ,feedDescription =
           Just "Productivity hacks weekly in bite-size chunks. Just up your alley if you&#39;re <br/>at least slightly geeky or a tad bit obsessed with Evernote or WorkFlowy!"
        ,feedFavicon = Nothing
        ,feedFormat = Just RSS2
        ,feedGenerator =
           Just "Site-Server v6.0.0-8783-8783 (http://www.squarespace.com)"
        ,feedGuid = Just "http://www.productivitymashup.com/"
        ,feedId = Nothing
        ,feedImage = Nothing
        ,feedLanguage = Just "en-US"
        ,feedLastPostDate = Nothing
        ,feedLastReadDate = Nothing
        ,feedLastReadStatus = Nothing
        ,feedLink = Just "http://www.productivitymashup.com/"
        ,feedPostCount = 0
        ,feedTitle = "Productivity Mashup"
        ,feedUri = empty}]

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
         ,postSummary =
            Just "How do Americans get ready to work with Russians aboard the International Space Station? They take a crash course in culture, language and protocol at Russia's <a href=\"http://howe.iki.rssi.ru/GCTC/gctc_e.htm\">Star City</a>."
         ,postTitle = Just "Star City"}
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
         ,postSummary =
            Just "Sky watchers in Europe, Asia, and parts of Alaska and Canada will experience a <a href=\"http://science.nasa.gov/headlines/y2003/30may_solareclipse.htm\">partial eclipse of the Sun</a> on Saturday, May 31st."
         ,postTitle = Nothing}
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
         ,postSummary =
            Just "Before man travels to Mars, NASA hopes to design new engines that will let us fly through the Solar System more quickly.  The proposed VASIMR engine would do that."
         ,postTitle = Just "The Engine That Does More"}
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
         ,postSummary =
            Just "Compared to earlier spacecraft, the International Space Station has many luxuries, but laundry facilities are not one of them.  Instead, astronauts have other options."
         ,postTitle = Just "Astronauts' Dirty Laundry"}]
  ,[Post {postAuthor = Just "Lea Verou"
         ,postComments = Nothing
         ,postDate = "Fri, 26 Aug 2016 20:07:58 +0000"
         ,postDescription =
            Just "<p><a href=\"http://lea.verou.me/wp-content/uploads/2016/08/Screen-Shot-2016-08-26-at-17.09.24.png\"><img class=\"alignright size-medium wp-image-2651\" src=\"http://lea.verou.me/wp-content/uploads/2016/08/Screen-Shot-2016-08-26-at-17.09.24-300x234.png\" alt=\"Screen Shot 2016-08-26 at 17.09.24\" width=\"300\" height=\"234\" srcset=\"http://lea.verou.me/wp-content/uploads/2016/08/Screen-Shot-2016-08-26-at-17.09.24-300x234.png 300w, http://lea.verou.me/wp-content/uploads/2016/08/Screen-Shot-2016-08-26-at-17.09.24-768x599.png 768w, http://lea.verou.me/wp-content/uploads/2016/08/Screen-Shot-2016-08-26-at-17.09.24.png 1021w\" sizes=\"(max-width: 300px) 100vw, 300px\" /></a>I have often lamented how many\160JavaScript developers don&#8217;t realize that\160a large percentage of\160HTML &amp; CSS authors are not comfortable writing JS, and struggle to use their libraries.</p><p>To encourage libraries with HTML APIs, i.e. libraries that can be used without writing a line of JS, I made a website to list and promote them: <a href=\"http://markapp.io\">markapp.io</a>. The list is\160currently quite short, so I\8217m counting on you to <a href=\"https://github.com/LeaVerou/markapp\">expand it</a>. Seen any libraries with good HTML APIs? Add them!</p><img src=\"http://feeds.feedburner.com/~r/leaverou/~4/7ilYG6MJKUI\" height=\"1\" width=\"1\" alt=\"\"/>"
         ,postFeedId = Nothing
         ,postGuid = "http://lea.verou.me/?p=2649"
         ,postImage = Nothing
         ,postLink = "http://feedproxy.google.com/~r/leaverou/~3/7ilYG6MJKUI/"
         ,postPubdate = Just "Fri, 26 Aug 2016 20:07:58 +0000"
         ,postSummary =
            Just "I have often lamented how many\160JavaScript developers don&#8217;t realize that\160a large percentage of\160HTML &#38; CSS authors are not comfortable writing JS, and struggle to use their libraries. To encourage libraries with HTML APIs, i.e. libraries that can be used without writing a line of JS, I made a website to list and promote them: markapp.io. [&#8230;]"
         ,postTitle = Just "Markapp: A list of HTML libraries"}
   ,Post {postAuthor = Just "Lea Verou"
         ,postComments = Nothing
         ,postDate = "Tue, 31 May 2016 22:13:00 +0000"
         ,postDescription =
            Just "<p><a href=\"http://lea.verou.me/wp-content/uploads/2016/05/multirange.png\"><img class=\"size-medium wp-image-2624 alignleft\" src=\"http://lea.verou.me/wp-content/uploads/2016/05/multirange-300x206.png\" alt=\"multirange\" width=\"300\" height=\"206\" srcset=\"http://lea.verou.me/wp-content/uploads/2016/05/multirange-300x206.png 300w, http://lea.verou.me/wp-content/uploads/2016/05/multirange-768x528.png 768w, http://lea.verou.me/wp-content/uploads/2016/05/multirange-1024x704.png 1024w, http://lea.verou.me/wp-content/uploads/2016/05/multirange.png 1165w\" sizes=\"(max-width: 300px) 100vw, 300px\" /></a>As part of my preparation for <a href=\"http://cssday.nl/2016/programme#lea-verou\">my talk at CSSDay HTML Special</a>, I was perusing\160the most recent HTML specs (<a href=\"https://html.spec.whatwg.org/multipage/\">WHATWG Living Standard</a>, <a href=\"https://www.w3.org/TR/html51/\">W3C HTML 5.1</a>) to see what undiscovered gems lay there. It turns out that HTML sliders have a lot of cool features specced that aren&#8217;t very well implemented:</p><ul><li>Ticks that snap via the <code>list</code> attribute and the <code>&lt;datalist&gt;</code> element. This is fairly decently implemented, except labelled ticks, which is not supported anywhere.</li><li>Vertical sliders when height &gt; width, implemented nowhere (instead, browsers employ proprietary ways for making sliders vertical: An <code>orient=vertical</code> attribute in Gecko, <code>-webkit-appearance: slider-vertical;</code> in WebKit/Blink and\160<code>writing-mode: bt-lr;</code> in IE/Edge). Good ol&#8217; rotate transforms work too, but have the usual problems, such as layout not being affected by the transform.</li><li>Two-handle sliders for ranges, via the <code>multiple</code> attribute.</li></ul><p>I made a quick <a href=\"http://dabblet.com/gist/0b79583e6e9c4e5e52aec5d682ac71d2\">testcase</a> for all three, and to my disappointment (but not to my surprise), support was extremely poor. I was most excited about the last one, since I&#8217;ve been wanting range sliders in HTML for a long time. Sadly, there are\160no implementations. But hey, what if I could create a polyfill by cleverly overlaying two sliders? Would it be possible? I started <a href=\"http://jsbin.com/risiki/edit?html,css,js,output\">experimenting in JSBin</a> last night, just for the lolz, then soon realized this could actually work and <a href=\"https://github.com/leaverou/multirange\">started a GitHub repo</a>. Since CSS variables are now supported almost everywhere, I&#8217;ve had a lot of fun using them. Sure, I could get broader support without them, but the code is much simpler, more elegant and customizable now. I also originally started with a <a href=\"http://blissfuljs.com\">Bliss</a> dependency, but realized it wasn&#8217;t worth it for such a tiny\160script.</p><p>So, enjoy, and contribute!</p><p><a class=\"view-demo\" href=\"http://leaverou.github.io/multirange/\">Multirange</a></p><img src=\"http://feeds.feedburner.com/~r/leaverou/~4/CgEmFMNXTjI\" height=\"1\" width=\"1\" alt=\"\"/>"
         ,postFeedId = Nothing
         ,postGuid = "http://lea.verou.me/?p=2621"
         ,postImage = Nothing
         ,postLink = "http://feedproxy.google.com/~r/leaverou/~3/CgEmFMNXTjI/"
         ,postPubdate = Just "Tue, 31 May 2016 22:13:00 +0000"
         ,postSummary =
            Just "As part of my preparation for my talk at CSSDay HTML Special, I was perusing\160the most recent HTML specs (WHATWG Living Standard, W3C HTML 5.1) to see what undiscovered gems lay there. It turns out that HTML sliders have a lot of cool features specced that aren&#8217;t very well implemented: Ticks that snap via the [&#8230;]"
         ,postTitle =
            Just "Introducing Multirange: A tiny polyfill for HTML5.1 two-handle sliders"}
   ,Post {postAuthor = Just "Lea Verou"
         ,postComments = Nothing
         ,postDate = "Thu, 17 Dec 2015 02:55:28 +0000"
         ,postDescription =
            Just "<p>Women speaking up about the sexism they have experienced in tech is great\160for raising awareness about the issues. However, <strong>when no positive stories get out, the overall picture painted is bleak, which could scare even more women away</strong>.</p><p>Lucky for me, <a href=\"http://lea.verou.me/2012/05/how-i-got-into-web-development-the-long-version/\">I fell in love with programming a decade\160before I even heard there is a sexism problem in tech</a>. Had I read about it before, I might have decided to go for some other profession.\160Who wants\160to be fighting an uphill battle all her\160life?</p><p>Thankfully, my experience has been quite different. Being in this industry has brought me nothing but happiness. Yes, there are several women who have had terrible experiences, and I\8217m in no way discounting them. They may even be the majority, though I am not aware of any statistics. However, there is also the other side. Those of us who have had incredibly positive experiences, and have always been treated with nothing but respect. That side\8217s stories need to be heard too, not silenced out of fear that we will become complacent and stop trying for more equality. Stories like mine should become the norm, not the exception.</p><p>I\8217ve had a number of different roles in tech over the course of my life. I\8217ve been a student, a speaker &amp; author, I\8217ve worked at <a href=\"http://w3.org\">W3C</a>, I\8217ve started &amp; maintain several successful open source projects and I\8217m <a href=\"http://lea.verou.me/2014/02/im-going-to-mit/\">currently dabbling in Computer Science research</a>. In none of these roles did I ever feel I was unfairly treated due to my gender. That is not because I\8217m oblivious to sexism.\160I tend to be very sensitive to seeing it, and I often notice even the smallest acts of sexism (\8220death by a thousand paper cuts\8221). I see a lot of sexism in society\160overall. However, inside this industry, my gender never seemed to matter much, except perhaps in positive ways.</p><p>On <a href=\"http://github.com/leaverou\">my open source repos</a>, I have several contributors, the overwhelming majority of which, is male. I\8217ve never felt less respected due to my gender. I\8217ve never felt that my work was taken less seriously than male OSS developers. I\8217ve never felt my contributors would not listen to me. I\8217ve never felt my work was unfairly scrutinized. Even when I didn\8217t know something, or introduced a horrible bug, I\8217ve never been insulted or berated. The community has been nothing but friendly, helpful and respectful. If anything, I\8217ve sometimes wondered if my gender is the reason I hardly ever get any shit!</p><p>On stage, I\8217ve never gotten any negative reactions. My talks always get excellent reviews, which have nothing to do with me being female. There is sometimes the odd complimentary tweet about my looks, but that\8217s not only\160exceedingly\160rare, but also always combined with a compliment about the actual talk content. My gender only affected my internal motivation: I often felt I <strong>had</strong>\160to be good, otherwise I would be painting all female tech speakers in a negative light. But other people are not at fault for my own stereotype threat.</p><p>My book, <a href=\"http://www.amazon.com/CSS-Secrets-Solutions-Everyday-Problems/dp/1449372635/ref=cm_cr_pr_product_top?ie=UTF8\">CSS Secrets</a>, has been as successful as an advanced CSS book could possibly aspire to be and got to an average of 5\160stars on Amazon only a few months after its release. It\8217s steadily the 5th bestseller on CSS and was No 1 for a while shortly after publication. My gender did not seem to negatively affect any of that, even though there\8217s a picture of me in the french flap so there are no doubts about me being female (as if the name Lea wasn\8217t enough of a hint).</p><p>As a student, I\8217ve never felt unfairly treated due to my gender by any of my professors, even the ones in Greece, a country that is not particularly famous for its gender equal society, to put it mildly.</p><p>As a new researcher, I have no experience with publishing papers yet, so I cannot share any\160experiences on that. However, I\8217ve been treated with nothing but respect by both <a href=\"https://en.wikipedia.org/wiki/David_Karger\">my advisor</a> and colleagues. My opinion is always heard and valued and even when people don\8217t agree, I can debate it as long and as intensely as I want, without being seen as aggressive or \8220bossy\8221.</p><p>I\8217ve worked at <a href=\"http://w3.org\">W3C</a> and still participate as an Invited Expert in the CSS Working Group. In neither of these roles did my gender seem to matter in any way. I\8217ve always felt that my expertise and skillset were valued and my opinions heard. In fact, the most well-respected member of the CSS WG, is the only other woman in it: <a href=\"http://fantasai.inkedblade.net/\">fantasai</a>.</p><p>Lastly, In all my years as a working professional, I\8217ve always negotiated any kind of remuneration, often\160hard. I\8217ve never lost an opportunity because of it, or been treated with negativity afterwards.</p><p>On the flip side, sexism today is rarely overt. Given that hardly anybody over ten will flat out admit they think women are inferior (even to themselves), it\8217s often hard to tell when a certain behavior stems from sexist beliefs. If someone is a douchebag\160to you, are they doing it because you\8217re a woman, or because they\8217re douchebags? If someone is criticizing your work, are they doing it because they genuinely found\160something to criticize or because they\8217re negatively predisposed due to your gender? It\8217s impossible to know, especially since <strong>they don\8217t know either</strong>! If you confront them on their sexism, they will deny all of it, and truly believe it. <strong>It takes a lot of introspection to see one\8217s internalized stereotypes.</strong> Therefore, a lot of the time, you cannot be sure if you have experienced sexist behavior, and there is no way to find out for sure, since the perpetrator doesn\8217t know either. There are many false positives and false negatives there.</p><p>Perhaps I don\8217t feel I have experienced much sexism because I prefer to err on the side of false negatives.\160Paraphrasing <a href=\"https://en.wikipedia.org/wiki/Blackstone%27s_formulation\">Blackstone</a>, I would rather not call out sexist behavior ten times, than wrongly accuse someone of it once. It might also have to do with my personality: I\8217m generally confident and can be very assertive. When somebody is being a jerk to me, I will not curl in a ball and question my life choices, I will reply to them in the same tone. However, those two alone cannot make the difference between a pit\160rampant with\160sexism and an\160egalitarian paradise. I think a lot of it is that we have genuinely\160made progress, and we should celebrate it\160with more women coming out with their positive experiences (it cannot just be me, right?).</p><p>Ironically, one of the very few times I have experienced any sexism in the industry was when a dude was trying to be nice to me. I was in a speaker room at a conference in Las Vegas, frantically working on my slides, not participating in any of the conversations around me. At some point, one of the guys said \8220fuck\8221 in a conversation, then turned and apologized to me. Irritated about the sudden interruption, I lifted my head and looked around. <strong>I noticed for the first time that day that I was the only woman in the room.</strong><strong>His effort to be courteous made me feel that I was different, the odd one out</strong>, the one we must be careful around and treat like a fragile flower. To this day, I regret being too startled to reply <em>\8220Eh, I don\8217t give a fuck\8221</em>.</p><img src=\"http://feeds.feedburner.com/~r/leaverou/~4/h62mIYhVziE\" height=\"1\" width=\"1\" alt=\"\"/>"
         ,postFeedId = Nothing
         ,postGuid = "http://lea.verou.me/?p=2590"
         ,postImage = Nothing
         ,postLink = "http://feedproxy.google.com/~r/leaverou/~3/h62mIYhVziE/"
         ,postPubdate = Just "Thu, 17 Dec 2015 02:55:28 +0000"
         ,postSummary =
            Just "Women speaking up about the sexism they have experienced in tech is great\160for raising awareness about the issues. However, when no positive stories get out, the overall picture painted is bleak, which could scare even more women away. Lucky for me, I fell in love with programming a decade\160before I even heard there is a [&#8230;]"
         ,postTitle = Just "My positive experience as a woman in tech"}]]
