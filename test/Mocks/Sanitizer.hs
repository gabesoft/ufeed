{-# LANGUAGE QuasiQuotes #-}

-- | Data for Sanitizer tests
module Mocks.Sanitizer where

import Text.RawString.QQ

baseUrl :: String
baseUrl = "http://mypost.com"

iframeExpected :: String
iframeExpected = "<div></div>"

iframeInput :: String
iframeInput =
  [r|
<html>
  <iframe width="560" height="315" src="https://www.youtube.com/embed/m5_AKjDdqaU" frameborder="0" allowfullscreen="" class="carousel-slide" data-carousel-index="6" tabindex="-1" style="width: 769px;"></iframe>
  <div></div>
</html>
|]

scriptExpected :: String
scriptExpected = "<div></div>"

scriptInput :: String
scriptInput =
  [r|
<html>
  <script src="http://code.jquery.com/jquery-migrate-1.2.1.min.js"></script>
  <div></div>
</html>
|]

stylesheetExpected :: String
stylesheetExpected = "<div></div>"

stylesheetInput :: String
stylesheetInput =
  [r|
<html>
  <link href="main.css" rel="stylesheet"/>
  <link href="other.css" rel="stylesheet"/>
  <div></div>
</html>
|]

styleExpected :: String
styleExpected = "<head></head><div></div>"

styleInput :: String
styleInput =
  [r|
<html>
    <head>
      <style type="text/css"> .wmd-snippet-button span { background-position: 0 0; } .wmd-snippet-button:hover span { background-position: 0 -40px; }</style>
    </head>
    <div></div>
</html>
|]

imageExpected =
  [r|<img width="230" src="http://mypost.com/u/2937359?v=3&amp;s=460"/><img width="230" src="http://mypost.com/2937359?v=3&amp;s=460"/><img width="230" src="http://placehold.it/350x150"/><div></div>|]

imageInput =
  [r|
<html>
  <img width="230" src="u/2937359?v=3&amp;s=460">
  <img width="230" src="/2937359?v=3&amp;s=460">
  <img width="230" src="http://placehold.it/350x150">
  <div></div>
</html>
|]

urlExpected =
  [r|<a href="http://mypost.com/u/2937359?v=3&amp;s=460">Link</a><a href="http://mypost.com/2937359?v=3&amp;s=460">Link</a><a href="http://placehold.it/350x150">Link</a><div></div>|]

urlInput =
  [r|
<html>
  <a href="u/2937359?v=3&amp;s=460">Link</a>
  <a href="/2937359?v=3&amp;s=460">Link</a>
  <a href="http://placehold.it/350x150">Link</a>
  <a href="http://github.com"></a>
  <div><a href="http://hello.com"></a></div>
</html>
|]