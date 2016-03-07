{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import ConeServer.ConeTypes
import Data.Aeson
import Data.ByteString.Lazy.Char8  as B   (readFile, ByteString)
import Data.Text as T                     (Text, pack)
import Types

getTrendingJSON :: IO ByteString
getTrendingJSON = B.readFile "trending.json"

parseTrendingHashtags :: ByteString -> Maybe TrendingHashtags
parseTrendingHashtags json = decode json :: Maybe TrendingHashtags

getHashtagStatsJSON :: IO ByteString
getHashtagStatsJSON = B.readFile "hashtag_stats.json"

parseHashtagStats :: ByteString -> Maybe HashtagStats
parseHashtagStats json = decode json :: Maybe HashtagStats

getConeEntryFromHashtag :: Hashtag -> ConeEntry
getConeEntryFromHashtag hashtag = ConeEntry {
  ceEntryId       = 0,
  ceLabel         = T.pack $ tag hashtag,
  ceTargetUri     = Just $ T.pack $ ("https://twitter.com/search?q=%23test" ++ tag hashtag),
  ceComment       = Nothing,
  ceIconName      = Nothing,
  ceStlName       = Nothing,
  ceColor         = Nothing,
  ceIsLeaf        = True,
  ceTextId        = T.pack $ tag hashtag
}

main = do
  putStr "\nTrending Hashtags\n"
  trendingJSON <- getTrendingJSON
  print $ parseTrendingHashtags trendingJSON
