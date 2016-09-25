{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Web server app
module Main where

import qualified Api
import           Blaze.ByteString.Builder (copyByteString, fromByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromShow)
import           Control.Exception
import           Control.Monad (join)
import           Data.Aeson (encode)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Lazy (fromStrict, toStrict)
import           Data.ByteString.UTF8 (fromString)
import           Data.Maybe
import           Data.Monoid ((<>))
import           Data.Text (pack, unpack)
import qualified FeedUpdater as F
import           Network.HTTP.Client (HttpException(..))
import           Network.HTTP.Types
       (status200, status201, status304, status400, status403, status404,
        statusCode, Status)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.RequestLogger
import           System.Environment (getArgs, lookupEnv)
import           System.Exit
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Hamlet (HtmlUrl, hamlet, shamlet)
import           Types

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
      let devEnv = maybe False (== "development") runEnv
      let requestLog =
            if devEnv
              then logStdoutDev
              else logStdout
      putStrLn $ "Listening on port " ++ show port
      run port (requestLog $ app h)
    _ -> putStrLn usage >> exitFailure

app :: String -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app apiHost req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", ["feeds"]) -> join (respond <$> getFeed apiHost req)
    _ -> respond renderIndex

updateFeed :: String
           -> String
           -> IO (Either SomeException (Bool, (Feed, [Post])))
updateFeed host uri = do
  maybeFeed <- Api.fetchFeedByUri host (pack uri)
  case maybeFeed of
    Left err ->
      if noResultsError err
        then fmap ((,) True) <$> update (nullFeed $ pack uri)
        else return (Left err)
    Right feed -> do
      results <- update feed
      case results of
        Left err ->
          if notModifiedError err
            then return (Right (False, (feed, [])))
            else return (Left err)
        Right _ -> return ((,) False <$> results)
  where
    update = F.update env
    env = F.envForAddNew host

noResultsError :: SomeException -> Bool
noResultsError err =
  case (fromException err :: Maybe Api.SearchException) of
    Just (Api.NoResultsException _) -> True
    _ -> False

notModifiedError :: SomeException -> Bool
notModifiedError = errorHasStatus status304

notFoundError :: SomeException -> Bool
notFoundError = errorHasStatus status404

errorHasStatus :: Status -> SomeException -> Bool
errorHasStatus st err =
  case (fromException err :: Maybe HttpException) of
    Just (StatusCodeException status _ _) -> status == st
    _ -> False

feedSuccessResponse :: Status -> Feed -> Response
feedSuccessResponse status feed =
  responseBuilder
    status
    [(hContentType, "application/json")]
    (copyByteString $ toStrict $ encode feed)

feedErrorResponse :: Status -> ByteString -> Response
feedErrorResponse status err =
  responseBuilder status [(hContentType, "text/plain")] (fromShow err)

getFeed :: String -> Request -> IO Response
getFeed host req =
  case uri of
    Nothing -> return $ feedErrorResponse status400 "A uri is required"
    Just u -> do
      results <- updateFeed host (BC.unpack u)
      case results of
        Left e ->
          if notFoundError e
            then return $ feedErrorResponse status404 (BC.pack $ show e)
            else return $ feedErrorResponse status400 (BC.pack $ show e)
        Right (created, (f, _)) ->
          return $ feedSuccessResponse (status created) f
  where
    query = queryString req
    uri = join $ lookup "uri" query
    status created =
      if created
        then status201
        else status200

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
    <title>Index
  <body>
    <h4>Feed reader
    <h5>Fetch a feed and it's posts, store and index it, and return it as json
    <p>/feeds?uri="feed-uri"
|]
