-- | Utility functions
module Util where

import Control.Applicative ((<|>))
import Control.Exception
import Data.Text (Text, pack, unpack)
import Data.Time
import Data.Time.ISO8601
import Network.HTTP.Client (HttpException(..))
import Network.HTTP.Types
       (status200, status201, status304, status400, status403, status404,
        statusCode, Status)

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

notModifiedError :: SomeException -> Bool
notModifiedError = errorHasStatus status304

notFoundError :: SomeException -> Bool
notFoundError = errorHasStatus status404

errorHasStatus :: Status -> SomeException -> Bool
errorHasStatus st err =
  case (fromException err :: Maybe HttpException) of
    Just (StatusCodeException status _ _) -> status == st
    _ -> False
