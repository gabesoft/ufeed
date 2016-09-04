-- | Convert rss feeds and posts
module Converter.RSS where

import Model
import qualified Text.RSS.Syntax as R

extractRSSFeed :: R.RSS -> Feed
extractRSSFeed feed =
  Feed {feedAuthor = R.rssEditor channel
       ,feedData = Nothing
       ,feedDate = R.rssLastUpdate channel
       ,feedDescription = Just $ R.rssDescription channel
       ,feedFavicon = Nothing
       ,feedGenerator = R.rssGenerator channel
       ,feedGuid = Just $ R.rssLink channel
       ,feedId = Nothing
       ,feedImage =
          Image . R.rssImageTitle <$> image <*> (R.rssImageURL <$> image)
       ,feedLanguage = Nothing
       ,feedLastPostDate = Nothing
       ,feedLastReadDate = Nothing
       ,feedLastReadStatus = Just ReadSuccess
       ,feedLink = R.rssLink channel
       ,feedOriginalDescription = Nothing
       ,feedPostCount = 0
       ,feedTitle = R.rssTitle channel
       ,feedUri = Nothing}
  where channel = R.rssChannel feed
        image = R.rssImage channel
