{-# LANGUAGE OverloadedStrings #-}

-- | Tests for FeedUpdater
module Main
  ( main
  ) where

import Control.Arrow
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (unpack, pack)
import Data.Time
import Data.Time.ISO8601 (parseISO8601)
import FeedUpdater
import qualified Mocks.FeedUpdater as M
import Test.Hspec
import Types
import Util

main :: IO ()
main = do
  bytes1 <- BL.readFile "test/data/atom1.0.sample5.xml"
  bytes2 <- BL.readFile "test/data/atom1.0.sample6.xml"
  now <- getCurrentTime
  let initState1 = UpdateState (Just now) initFeed initPosts []
      initState2 = UpdateState (Just now) initFeed initPosts []
  (_, state1) <-
    runUpdateM (envForUpdate "") initState1 $ processFeed (bytes1, M.modified)
  (_, state2) <-
    runUpdateM (envForUpdate "") initState2 $ processFeed (bytes2, M.modified)
  hspec $
    describe "process feeds" $
    do it "sets the last modified date" $ verifyModified state1
       it "sets the last updated date" $ verifyUpdated state1 now
       it "sets the last post date" $ verifyLastPostDate state1
       it "sets the post count" $ verifyLastPostCount state1
       it "sets the feed uri" $ verifyFeedUri state1
       it "sets the read status" $ verifyReadStatus state1
       it "extracts all the new posts" $ verifyNewPostCount state1
       it "formats all dates to ISO8601" $ verifyNewPostDates state1
       it "resets the failed attempts to 0" $ verifyFailedAttempts state1
       it "sets the feed link when populated" $ verifyLinkExisting state1
       it "sets the feed link to uri when empty" $ verifyLinkEmpty state2

verifyLinkEmpty :: UpdateState -> Expectation
verifyLinkEmpty state =
  state ^. updateFeed ^. to feedLink `shouldBe` Just (feedUri M.feed)

verifyLinkExisting :: UpdateState -> Expectation
verifyLinkExisting state =
  state ^. updateFeed ^. to feedLink `shouldBe` feedLink M.feed

verifyFailedAttempts :: UpdateState -> Expectation
verifyFailedAttempts state =
  state ^. updateFeed ^. to feedFailedAttempts `shouldBe` 0

verifyReadStatus :: UpdateState -> Expectation
verifyReadStatus state =
  state ^. updateFeed ^. to feedLastReadStatus `shouldBe` Just ReadSuccess

verifyFeedUri :: UpdateState -> Expectation
verifyFeedUri state =
  state ^. updateFeed ^. to feedUri `shouldBe`
  pack "https://www.reddit.com/r/emacs/.rss"

verifyLastPostCount :: UpdateState -> Expectation
verifyLastPostCount state =
  state ^. updateFeed ^. to feedPostCount `shouldBe` length M.posts

verifyLastPostDate :: UpdateState -> Expectation
verifyLastPostDate state =
  state ^. updateFeed ^. to feedLastPostDate `shouldBe`
  Just (pack "2016-09-11T16:36:34.000Z")

verifyNewPostDates :: UpdateState -> Expectation
verifyNewPostDates state = length (catMaybes dates) `shouldBe` count
  where
    dates = fmap (parseISO8601 . unpack) . postDate <$> _latestPosts state
    count = state ^. latestPosts ^. to length

verifyNewPostCount :: UpdateState -> Expectation
verifyNewPostCount state = count `shouldBe` length M.posts - length initPosts
  where
    count = state ^. latestPosts ^. to length

verifyUpdated :: UpdateState -> UTCTime -> Expectation
verifyUpdated state now = actual `shouldBe` Just (formatJsDate now)
  where
    actual = state ^. updateFeed ^. to feedLastReadDate

verifyModified :: UpdateState -> Expectation
verifyModified state = actual `shouldBe` Just M.modified
  where
    actual = state ^. updateFeed ^. to feedLastModified

initFeed :: Feed
initFeed =
  feed
  { feedId = feedId M.feed
  , feedFailedAttempts = 5
  }
  where
    feed = nullFeed (feedUri M.feed)

initPosts :: PostMap
initPosts = Map.fromList $ (postGuid &&& id) <$> posts
  where
    posts = take 2 M.posts
