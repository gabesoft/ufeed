{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Web server app
module Main where

import qualified Api
import Blaze.ByteString.Builder (copyByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import Control.Exception
import Control.Monad (join)
import Data.Aeson (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.UTF8 (fromString)
import Data.Maybe
import Data.Text (pack)
import qualified FeedUpdater as F
import Network.HTTP.Types
       (Status, status200, status201, status400, status404)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger
import System.Environment (getArgs, lookupEnv)
import System.Exit
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Hamlet (shamlet)
import Types
import Util

data UpdateResult = UpdateResult
  { feedCreated :: Bool
  , resultFeed :: Feed
  , resultPostCount :: Int
  } deriving (Eq, Show)

usage :: String
usage = "Usage: server <port> <api-host>"

main :: IO ()
main = do
  args <- getArgs
  runEnv <- lookupEnv "RUN_ENV"
  putStrLn $ "Input arguments: " ++ show args
  putStrLn $ "Run environment: " ++ fromMaybe "production" runEnv
  case args of
    p:h:_ -> do
      let port = read p
      let development = maybe False (== "development") runEnv
      let requestLog =
            if development
              then logStdoutDev
              else logStdout
      putStrLn $ "Listening on port " ++ show port
      run port (requestLog $ app h)
    _ -> putStrLn usage >> exitFailure

app :: String -> Application
app apiHost req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", ["feeds"]) -> join (respond <$> getFeed apiHost req)
    _ -> respond renderIndex

getFeed :: String -> Request -> IO Response
getFeed host req =
  case join $ lookup "uri" (queryString req) of
    Nothing -> return $ feedErrorResponse status400 "A uri is required"
    Just uri -> do
      results <- updateFeed host (BC.unpack uri)
      return $
        case results of
          Left e ->
            if notFoundError e
              then feedErrorResponse status404 (show e)
              else feedErrorResponse status400 (show e)
          Right (UpdateResult created feed _) ->
            feedSuccessResponse (status created) feed
  where
    status created =
      if created
        then status201
        else status200

updateFeed :: String -> String -> IO (Either SomeException UpdateResult)
updateFeed host uri = do
  maybeFeed <- Api.fetchFeedByUri host (pack uri)
  case maybeFeed of
    Left err ->
      if Api.isNoResultsError err
        then update True (nullFeed $ pack uri)
        else return (Left err)
    Right feed -> update False feed
  where
    mkResult created (f, ps) = UpdateResult created f (length ps)
    update created feed =
      fmap (mkResult created) <$> F.update (F.envForAddNew host) feed

feedSuccessResponse :: Status -> Feed -> Response
feedSuccessResponse status feed =
  responseBuilder
    status
    [(hContentType, "application/json")]
    (copyByteString $ toStrict $ encode feed)

feedErrorResponse :: Status -> String -> Response
feedErrorResponse status err =
  responseBuilder status [(hContentType, "text/plain")] (fromShow err)

renderIndex :: Response
renderIndex =
  responseBuilder
    status200
    [(hContentType, "text/html")]
    (mconcat $ copyByteString <$> [indexHtml])

indexHtml :: ByteString
indexHtml =
  fromString $
  renderHtml $
  [shamlet|
$doctype 5
<html>
  <head>
    <title>Methods Index
  <body>
    <h4>Feed reader
    <h5>Fetch a feed and it's posts, store and index it, and return it as json
    <p>/feeds?uri="feed-uri"
|]
