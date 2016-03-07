{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import ConeServer.ConeTypes
import Data.Aeson
import Data.ByteString.Lazy.Char8         as B (readFile, ByteString)
import Data.Text                          as T (Text, pack, append)
import Types

twitterSearchURL :: Text
twitterSearchURL = "https://twitter.com/search?q=%23"

getTrendingJSON :: IO ByteString
getTrendingJSON = B.readFile "trending.json"

parseTrendingHashtags :: ByteString -> Maybe TrendingHashtags
parseTrendingHashtags = decode

getHashtagStatsJSON :: IO ByteString
getHashtagStatsJSON = B.readFile "hashtag_stats.json"

parseHashtagStats :: ByteString -> Maybe HashtagStats
parseHashtagStats = decode

getConeEntryFromHashtag :: Hashtag -> ConeEntry
getConeEntryFromHashtag hashtag = ConeEntry {
  ceEntryId       = 0,
  ceLabel         = tag hashtag,
  ceTargetUri     = Just $ twitterSearchURL `T.append` tag hashtag,
  ceComment       = Nothing,
  ceIconName      = Nothing,
  ceStlName       = Nothing,
  ceColor         = Nothing,
  ceIsLeaf        = True,
  ceTextId        = tag hashtag
}

tagList :: Maybe TrendingHashtags -> [Hashtag]
tagList Nothing = []
tagList (Just th) = thTags th

main = do
  trendingJSON <- getTrendingJSON
  -- print $ parseTrendingHashtags trendingJSON
  print . map getConeEntryFromHashtag $ tagList $ parseTrendingHashtags trendingJSON
