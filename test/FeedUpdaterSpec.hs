-- | Tests for FeedUpdater
module Main (main) where

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

main :: IO ()
main =
  do bytes <- BL.readFile "test/data/atom1.0.sample5.xml"
     now <- getCurrentTime
     let initState = UpdateState now initFeed initPosts []
         res =
           processFeed initState
                       (bytes,M.modified)
         updatedState = either (const initState) id res
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

verifyReadStatus :: UpdateState -> Expectation
verifyReadStatus state =
  state ^. to updateFeed ^. to feedLastReadStatus `shouldBe` Just ReadSuccess

verifyFeedUri :: UpdateState -> Expectation
verifyFeedUri state =
  state ^. to updateFeed ^. to feedUri `shouldBe`
  pack "https://www.reddit.com/r/emacs/.rss"

verifyLastPostCount :: UpdateState -> Expectation
verifyLastPostCount state =
  state ^. to updateFeed ^. to feedPostCount `shouldBe` length M.posts

verifyLastPostDate :: UpdateState -> Expectation
verifyLastPostDate state =
  state ^. to updateFeed ^. to feedLastPostDate `shouldBe`
  Just (pack "2016-09-11T16:36:34.000Z")

verifyNewPostDates :: UpdateState -> Expectation
verifyNewPostDates state = length (catMaybes dates) `shouldBe` count
  where dates = parseISO8601 . unpack . postDate <$> newPosts state
        count = state ^. to newPosts ^. to length

verifyNewPostCount :: UpdateState -> Expectation
verifyNewPostCount state = count `shouldBe` length M.posts - length initPosts
  where count = state ^. to newPosts ^. to length

verifyUpdated
  :: UpdateState -> UTCTime -> Expectation
verifyUpdated state now = actual `shouldBe` Just (formatJsDate now)
  where actual = state ^. to updateFeed ^. to feedLastReadDate

verifyModified :: UpdateState -> Expectation
verifyModified state = actual `shouldBe` Just M.modified
  where actual = state ^. to updateFeed ^. to feedLastModified

initFeed :: Feed
initFeed = feed {feedId = feedId M.feed}
  where feed = nullFeed (feedUri M.feed)

initPosts :: PostMap
initPosts = Map.fromList $ (postGuid &&& id) <$> posts
  where posts = take 2 M.posts