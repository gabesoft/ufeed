{-# LANGUAGE OverloadedStrings #-}

-- | Interaction with the api server
module Api where

import Control.Lens
import Data.Text (pack)
import Model
import Network.Wreq

fetchFeeds :: String -> Int -> IO [Feed]
fetchFeeds host limit = (^. responseBody) <$> (getWith opts url >>= asJSON)
  where url = host ++ "/search/feeds"
        opts = defaults & param "limit" .~ [pack $ show limit]
