-- |
-- Updates a feed
-- Updating a feed consists of fetching the latest feed data,
-- uploading the updated feed and any new posts to database,
-- and indexing the new posts with the search engine
module FeedUpdater where

import Api
import Control.Applicative ((<|>))
import Control.Arrow
import Control.Exception
import Converter
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text, empty, pack, unpack)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format
       (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.Time.ISO8601 (formatISO8601Javascript, parseISO8601)
import FeedReader
import Types

type ApiHost = String

type ReadFlag = Bool

type PostMap = Map.Map Text Post

data UpdateEnv =
  UpdateEnv {apiHost :: String
            ,readFlag :: Bool}
  deriving (Eq,Show)

data UpdateState =
  UpdateState {updateTime :: UTCTime
              ,updateFeed :: Feed
              ,existingPosts :: PostMap
              ,newPosts :: [Post]}
  deriving (Eq,Show)

-- Monad stack : IO ReaderT StateT EitherT

update
  :: UpdateEnv -> Feed -> IO (Either SomeException (Feed,[Post]))
update env feed =
  do ut <- getCurrentTime
     ep <-
       readExistingPosts (apiHost env)
                         (unpack <$> feedId feed)
     case ep of
       Left e -> return $ Left e
       Right posts ->
         do st <- fetchFeedData (UpdateState ut feed posts [])
            case st of
              Left e -> return $ Left e -- TODO: save feed to db with an error last read status
              Right s -> return $ Right (updateFeed s,newPosts s)

readExistingPosts
  :: String -> Maybe String -> IO (Either SomeException PostMap)
readExistingPosts _ Nothing = return (Right Map.empty)
readExistingPosts host (Just fid) =
  do posts <- fetchPosts host fid
     return $ Map.fromList . fmap (postGuid &&& id) <$> posts

fetchFeedData
  :: UpdateState -> IO (Either SomeException UpdateState)
fetchFeedData state =
  do dt <- fetchFeed (unpack $ feedUri feed) modified
     return (dt >>= processFeed state)
  where feed = updateFeed state
        modified = fromMaybe nullLastModified (feedLastModified feed)

processFeed :: UpdateState
            -> (ByteString,LastModified)
            -> Either SomeException UpdateState
processFeed state (bytes,modified) =
  do (updated,posts) <- extract
     return state {newPosts = posts
                  ,updateFeed = updated {feedLastReadDate = Just now}}
  where feed = updateFeed state
        now = formatJsDate (updateTime state)
        existing = existingPosts state
        isNew guids post =
          not $
          Set.member (postGuid post)
                     guids
        sequenceT = fmap fst &&& fmap snd
        extract =
          do (f,ps) <- extractFeedAndPosts bytes
             let newps = filter (isNew $ Map.keysSet existing) ps
                 (posts,dates) =
                   sequenceT $ normalizePostDates (updateTime state) <$> newps
                 lastd = formatJsDate <$> maximumDate (catMaybes dates)
             return (f {feedGuid = feedGuid feed <|> feedGuid f
                       ,feedId = feedId feed
                       ,feedLastModified = Just modified
                       ,feedLastPostDate = lastd <|> feedLastPostDate feed
                       ,feedLastReadStatus = Just ReadSuccess
                       ,feedLink = feedGuid f <|> feedGuid feed
                       ,feedPostCount = Map.size existing + length posts
                       ,feedUri = feedUri feed}
                    ,posts)

maximumDate :: [UTCTime] -> Maybe UTCTime
maximumDate [] = Nothing
maximumDate xs = Just (maximum xs)

formatJsDate :: UTCTime -> Text
formatJsDate = pack . formatISO8601Javascript

normalizePostDates
  :: UTCTime -> Post -> (Post,Maybe UTCTime)
normalizePostDates now post =
  (post {postDate = fromJust (t1 <|> Just (postDate post))
        ,postPubdate = t2 <|> postPubdate post}
  ,d1)
  where (t1,d1) =
          normalizeDate now
                        (postDate post)
        (t2,_) =
          normalizeDate now
                        (fromMaybe empty (postPubdate post))

normalizeDate
  :: UTCTime -> Text -> (Maybe Text,Maybe UTCTime)
normalizeDate now input = (formatJsDate <$> date,date)
  where date = (`min` now) <$> parseJsDate (unpack input)

parseJsDate :: String -> Maybe UTCTime
parseJsDate input = parseISO8601 input <|> parseRFC822 input

parseRFC822 :: String -> Maybe UTCTime
parseRFC822 = parseTimeM True defaultTimeLocale rfc822DateFormat