-- | Utility functions
module Util where

import Control.Applicative ((<|>))
import Data.Text (Text, pack, unpack)
import Data.Time
import Data.Time.ISO8601

getCurrentTimeJs :: IO String
getCurrentTimeJs = formatISO8601Javascript <$> getCurrentTime

formatJsDate :: UTCTime -> Text
formatJsDate = pack . formatISO8601Javascript

normalizeDate :: UTCTime -> Text -> (Maybe Text, Maybe UTCTime)
normalizeDate now input = (formatJsDate <$> date, date)
  where
    date = (`min` now) <$> parseJsDate (unpack input)

parseJsDate :: String -> Maybe UTCTime
parseJsDate input = parseISO8601 input <|> parseRFC822 input

parseRFC822 :: String -> Maybe UTCTime
parseRFC822 = parseTimeM True defaultTimeLocale rfc822DateFormat
