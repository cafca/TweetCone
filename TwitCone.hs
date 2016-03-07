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

thTagList :: Maybe TrendingHashtags -> [Hashtag]
thTagList Nothing = []
thTagList (Just th) = thTags th

hsTagList :: Maybe HashtagStats -> [Hashtag]
hsTagList Nothing = []
hsTagList (Just hs) = hsAssociatedHashtags hs

extractRelated :: ByteString -> [ConeEntry]
extractRelated json = let
  hs = parseHashtagStats json
  tags = hsTagList hs
  in map getConeEntryFromHashtag tags

extractTrending :: ByteString -> [ConeEntry]
extractTrending json = let
  th = parseTrendingHashtags json
  tags = thTagList th
  in map getConeEntryFromHashtag tags

main = do
  putStr "\nRelated Tags\n"
  statsJSON <- getHashtagStatsJSON
  print $ extractRelated statsJSON

  putStr "\nTrending Tags\n"
  trendingJSON <- getTrendingJSON
  print $ extractTrending trendingJSON
