{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Query.RSS
module Main (main) where

import Converter.RSS
import Mocks.RSS
import Test.Hspec
import TestHelper

main :: IO ()
main =
  hspec $
  do describe "extract feed data" $ mapM_ runFeed feedCases
     describe "extract post data" $ mapM_ runPosts postsCases
  where runFeed (f,e) = it f $ extractAndVerifyFeed extractFeed f e
        runPosts (f,e) = it f $ extractAndVerifyPosts extractPosts f e
