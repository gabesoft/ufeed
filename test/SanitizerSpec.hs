-- | Tests for Sanitizer
module Main
  ( main
  ) where

import Mocks.Sanitizer
import Sanitizer
import Test.Hspec

main :: IO ()
main =
  hspec $
  describe "cleans up html" $
  do it "removes script tags" $ verify scriptInput scriptExpected
     it "removes stylesheets" $ verify stylesheetInput stylesheetExpected
     it "removes style tags" $ verify styleInput styleExpected
     it "fixes image urls" $ verify imageInput imageExpected
     it "fixes urls and removes empty links" $ verify urlInput urlExpected
     it "can extract content" $ verifyContent validContentInput validContentExpected
     it "can handle malformed html" $ verifyContent malformedHtmlInput malformedHtmlExpected
     it "fixes image srcset" $ verify srcsetInput srcsetExpected
     it "removes share links" $ verify shareInput shareExpected
     it "does not alter code blocks" $ verify codeBlockInput codeBlockExpected

verify :: String -> String -> Expectation
verify input expected = sanitize "" baseUrl input `shouldBe` expected

verifyContent :: String -> String -> Expectation
verifyContent input expected =
  sanitize "http://sample.for.test/feed" baseUrl input `shouldBe` expected
