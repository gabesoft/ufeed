-- | Utility functions
module Util where

import Data.Time
import Data.Time.ISO8601

ensureAbsoluteUrl :: String -> String -> String
ensureAbsoluteUrl baseUrl [] = baseUrl
ensureAbsoluteUrl baseUrl ('/':path) = ensurePathEnd baseUrl ++ path
  where ensurePathEnd [] = "/"
        ensurePathEnd xs
          | last xs == '/' = xs
          | otherwise = xs ++ "/"
ensureAbsoluteUrl _ a = a

getCurrentTimeJs :: IO String
getCurrentTimeJs = formatISO8601Javascript <$> getCurrentTime
