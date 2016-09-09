-- | Tests for Sanitizer
module Main (main) where

import Mocks.Sanitizer
import Sanitizer
import Test.Hspec

main :: IO ()
main =
  hspec $
  describe "cleans up html" $
  do it "removes iframes" $ verify iframeInput iframeExpected
     it "removes script tags" $ verify scriptInput scriptExpected
     it "removes stylesheets" $ verify stylesheetInput stylesheetExpected
     it "removes style tags" $ verify styleInput styleExpected
     it "fixes image urls" $ verify imageInput imageExpected
     it "fixes urls and removes empty links" $ verify urlInput urlExpected

verify :: String -> String -> Expectation
verify input expected = sanitize baseUrl input `shouldBe` expected