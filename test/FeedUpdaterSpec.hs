-- | Tests for FeedUpdater
module Main
  ( main
  ) where

import           Control.Arrow
import           Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (unpack, pack)
import           Data.Time
import           Data.Time.ISO8601 (parseISO8601)
import           FeedUpdater
import qualified Mocks.FeedUpdater as M
import           Test.Hspec
import           Types
import           Util

main :: IO ()
main = do
  bytes <- BL.readFile "test/data/atom1.0.sample5.xml"
  now <- getCurrentTime
  let state = UpdateState (Just now) initFeed initPosts []
  (_,st) <- runUpdateM (envForUpdate "") state $ processFeed (bytes, M.modified)
  let updatedState = st
  hspec $
    describe "process feeds" $
    do it "sets the last modified date" $ verifyModified updatedState
       it "sets the last updated date" $ verifyUpdated updatedState now
       it "sets the last post date" $ verifyLastPostDate updatedState
       it "sets the post count" $ verifyLastPostCount updatedState
       it "sets the feed uri" $ verifyFeedUri updatedState
       it "sets the read status" $ verifyReadStatus updatedState
       it "extracts all the new posts" $ verifyNewPostCount updatedState
       it "formats all dates to ISO8601" $ verifyNewPostDates updatedState
       it "resets the failed attempts to 0" $ verifyFailedAttempts updatedState

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
    dates = parseISO8601 . unpack . postDate <$> _latestPosts state
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
