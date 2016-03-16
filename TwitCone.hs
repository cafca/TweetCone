{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import ConeServer.RunServer
import ConeServer.ConeTypes
import ConeServer.Types                   (RoseTree(..), enumerateTree)
import ConeServer.Utils

import Network.OAuth.OAuth2.Internal      (AccessToken)
import Network.Wai.Handler.Warp           (Port)
import Data.Aeson
import Data.ByteString.Lazy.Char8         as B (readFile, ByteString)
import Data.Text                          as T (Text, pack, append)

import Control.Concurrent                 (threadDelay, forkIO)

import Types
import TwitterConnector

import Web.Twitter.Types                      (SearchResult(..))

baseDir :: FilePath
baseDir = "/Users/work/code/ConeServer"

srvPort :: Port
srvPort = 8080

twitterSearchURL :: Text
twitterSearchURL = "https://ritetag.com/hashtag-stats/"

domainLabel :: Text
domainLabel = "Trending Hashtags"

getJSON :: String -> IO ByteString
getJSON fname = B.readFile $ fname ++ ".json"

parseTrendingHashtags :: ByteString -> Maybe TrendingHashtags
parseTrendingHashtags = decode

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
  in fmap getConeEntryFromHashtag tags

extractTrending :: ByteString -> [ConeEntry]
extractTrending json = let
  th = parseTrendingHashtags json
  tags = thTagList th
  in fmap getConeEntryFromHashtag tags

node :: ConeEntry -> [ConeTree] -> ConeTree
node e [] = RoseLeaf e {ceIsLeaf = ceIsLeaf e && True, ceTextId = "tId_" `append` ceLabel e} (-1) []
node e cs = RoseLeaf e {ceIsLeaf = False, ceTextId = "tId_" `append` ceLabel e} (-1) cs

buildTwitCone :: [ConeEntry] -> [ConeEntry] -> ConeTree
buildTwitCone rs ts =
  RoseLeaf emptyLeaf {ceIsLeaf = False, ceTextId = "tId_root", ceLabel = domainLabel} (-1) $
    fmap (flip node (fmap (flip node []) rs)) ts

-- refreshModel :: IO AccessToken ->

main = do
  putStrLn "Constructing TwitCone"

  -- Read related hashtag samples from file
  statsJSON <- getJSON "hashtag_stats"
  trendingJSON <- getJSON "trending"

  let myTree = buildTwitCone (extractRelated statsJSON) (extractTrending trendingJSON)

  bearerToken <- requestToken

  -- Start server
  ioData <- initServer srvPort baseDir False bearerToken
  forkIO $ aktualisator ioData

  putStrLn $ "starting on localhost:" ++ show srvPort
  runServer ioData Nothing Nothing (enumerateTree coneEntrySetId 1 myTree)

aktualisator :: IOData AccessToken -> IO ()
aktualisator ioData = getCustom ioData >>= go
  where
    go bearerToken = do
      threadDelay $ 10 * 1000 * 1000
      putStrLn "Refreshing Twitter data..."
      -- mach Washington
      rt <- retrieveTrending bearerToken :: IO (Maybe (SearchResult String))
      let model' = emptyTree
      applyIOSetter ioData model' setTestModel
      go bearerToken
