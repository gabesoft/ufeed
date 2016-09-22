module Main where

import qualified Api
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Text (unpack)
import Data.Time
import FeedUpdater
import System.Environment (getArgs)
import System.Exit
import Text.Show.Pretty
import Types

delay :: Int
delay = 3600000000 -- 1 hour in microseconds

env :: String -> UpdateEnv
env = envForUpdate

usage :: String
usage = "updater <api-host>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn usage >> exitFailure
    (h:_) -> forever (updateFeeds h)

updateFeeds :: String -> IO ()
updateFeeds host = do
  startTime <- getCurrentTime
  startTimeL <- utcToLocalZonedTime startTime
  putStrLn $ show startTimeL ++ " Starting feeds update"
  updateMultipleFeeds host
  endTime <- getCurrentTime
  endTimeL <- utcToLocalZonedTime endTime
  putStrLn $ show endTimeL ++ " All feeds updated. Going to sleep for 1 hour"
  putStrLn $ show (diffUTCTime endTime startTime) ++ " total time"
  threadDelay delay

updateMultipleFeeds :: String -> IO ()
updateMultipleFeeds host = do
  results <- Api.fetchFeeds host 0
  case results of
    Left e -> do
      putStrLn "Failed to fetch feeds"
      pPrint e
    Right feeds -> do
      putStrLn $ "Found " ++ show (length feeds) ++ " feeds"
      mapM_ (updateSingleFeed host) feeds

updateSingleFeed :: String -> Feed -> IO ()
updateSingleFeed host feed = do
  putStrLn $ "Starting feed update " ++ unpack (feedUri feed)
  result <- update (env host) feed
  displayItem result

displayItem :: Either SomeException (Feed, [Post]) -> IO ()
displayItem (Left e) = pPrint e
displayItem (Right (f, ps)) = do
  putStrLn ("Feed updated " ++ unpack (feedUri f))
  putStrLn ("New posts found " ++ show (length ps))
