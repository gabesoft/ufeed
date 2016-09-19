module Main where

import qualified Api
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Data.Text (unpack)
import FeedUpdater
import Types
import Text.Show.Pretty

host :: String
host = "http://localhost:8006"

delay :: Int
delay = 3600000000 -- 1 hour in microseconds

env :: UpdateEnv
env = envForUpdate host

main :: IO ()
main = forever updateAllFeeds

updateAllFeeds :: IO ()
updateAllFeeds = do
  putStrLn "Starting feeds update"
  results <- Api.fetchFeeds host 0
  case results of
    Left e -> do
      putStrLn "Failed to fetch feeds"
      pPrint e
    Right feeds -> do
      putStrLn $ "Found " ++ show (length feeds) ++ " feeds"
      mapM_ updateFeedItem feeds
  --  void $ mapConcurrently updateFeedItem feeds
  putStrLn "All feeds updated. Going to sleep for 1 hour"
  threadDelay delay

updateFeedItem :: Feed -> IO ()
updateFeedItem feed = do
  putStrLn $ "Starting feed update " ++ unpack (feedUri feed)
  result <- update env feed
  displayItem result

display :: [Either SomeException (Feed, [Post])] -> IO ()
display = mapM_ displayItem

displayItem :: Either SomeException (Feed, [Post]) -> IO ()
displayItem (Left e) = pPrint e
displayItem (Right (f, ps)) = do
  putStrLn ("Feed updated " ++ unpack (feedUri f))
  putStrLn ("New posts found " ++ show (length ps))
