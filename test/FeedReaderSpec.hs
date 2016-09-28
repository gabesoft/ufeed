{-# LANGUAGE OverloadedStrings #-}

-- | Tests for FeedReader
module Main
  ( main
  ) where

import Data.ByteString
import FeedReader
import Test.Hspec

main :: IO ()
main =
  hspec $
  describe "feed reader" $
  do it "returns not latin for missing content type" $ verifyLatin Nothing False
     it "returns not latin for non-latin encoding" $
       verifyLatin (Just "text/xml; charset=utf-8") False
     it "returns latin for latin encoding" $
       verifyLatin (Just "text/xml; charset=ISO-8859-1") True

verifyLatin :: Maybe ByteString -> Bool -> Expectation
verifyLatin ctype expected = isLatin1 ctype `shouldBe` expected
