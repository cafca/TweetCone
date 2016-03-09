{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import ConeServer.RunServer
import ConeServer.ConeTypes
import ConeServer.Types                   (RoseTree(..), enumerateTree)

import Network.Wai.Handler.Warp           (Port)
import Data.Aeson
import Data.ByteString.Lazy.Char8         as B (readFile, ByteString)
import Data.Text                          as T (Text, pack, append)

import Types

baseDir :: FilePath
baseDir = "/Users/work/code/ConeServer"

srvPort :: Port
srvPort = 8080

twitterSearchURL :: Text
twitterSearchURL = "https://twitter.com/hashtag/"

domainLabel :: Text
domainLabel = "Trending Hashtags"

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

node :: ConeEntry -> [ConeTree] -> ConeTree
node e [] = RoseLeaf e {ceIsLeaf = ceIsLeaf e && True, ceTextId = "tId_" `append` ceLabel e} (-1) []
node e cs = RoseLeaf e {ceIsLeaf = False, ceTextId = "tId_" `append` ceLabel e} (-1) cs

buildTwitCone :: [ConeEntry] -> [ConeEntry] -> ConeTree
buildTwitCone rs ts =
  RoseLeaf emptyLeaf {ceIsLeaf = False, ceTextId = "tId_root", ceLabel = domainLabel} (-1) $
    map (\e -> node e (map (\e -> node e []) rs)) ts

main = do
  putStrLn "Constructing TwitCone"

  -- Read related hashtag samples from file
  statsJSON <- getHashtagStatsJSON
  let related = extractRelated statsJSON

  -- Build [ConeEntry] from JSON file of trending hashtags
  trendingJSON <- getTrendingJSON
  let myTree = buildTwitCone related $ extractTrending trendingJSON

  -- Start server
  ioData <- initServer srvPort baseDir False
  putStrLn $ "starting on localhost:" ++ show srvPort
  runServer ioData Nothing Nothing (enumerateTree coneEntrySetId 1 myTree)
