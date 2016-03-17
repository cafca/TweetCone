{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import ConeServer.RunServer
import ConeServer.ConeTypes
import ConeServer.Types                   (RoseTree(..), enumerateTree)
import ConeServer.Utils

import Network.OAuth.OAuth2.Internal      (AccessToken)
import Network.Wai.Handler.Warp           (Port)
import Data.Aeson
import Data.ByteString.Lazy.Char8         as B (readFile, ByteString, pack)
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
domainLabel = "Trending search queries"

getJSON :: String -> IO ByteString
getJSON fname = B.readFile $ fname ++ ".json"

parseResult :: ByteString -> Maybe Trending
parseResult = decode

getConeEntryFromQuery :: SearchQuery -> ConeEntry
getConeEntryFromQuery sq = ConeEntry {
  ceEntryId       = 0,
  ceLabel         = sqName sq,
  ceTargetUri     = Just $ sqURI sq,
  ceComment       = Nothing,
  ceIconName      = Nothing,
  ceStlName       = Nothing,
  ceColor         = Nothing,
  ceIsLeaf        = True,
  ceTextId        = sqName sq
}

trQueries :: Maybe Trending -> [SearchQuery]
trQueries Nothing = []
trQueries (Just tr) = trTrends tr

extractTrending :: ByteString -> [ConeEntry]
extractTrending json = let
    tags = trQueries $ parseResult json
  in fmap getConeEntryFromQuery tags

node :: ConeEntry -> [ConeTree] -> ConeTree
node e [] = RoseLeaf e {ceIsLeaf = ceIsLeaf e && True, ceTextId = "tId_" `append` ceLabel e} (-1) []
node e cs = RoseLeaf e {ceIsLeaf = False, ceTextId = "tId_" `append` ceLabel e} (-1) cs

buildTwitCone :: [ConeEntry] -> ConeTree
buildTwitCone ts =
  RoseLeaf emptyLeaf {ceIsLeaf = False, ceTextId = "tId_root", ceLabel = domainLabel} (-1) $
    fmap (flip node []) ts

-- refreshModel :: IO AccessToken ->

main = do
  putStrLn "Constructing TwitCone"

  -- Read related hashtag samples from file
  trendingJSON <- getJSON "trending"
  let myTree = buildTwitCone $ extractTrending trendingJSON

  -- Prepare server and fork updateJob
  bearerToken <- requestToken
  ioData <- initServer srvPort baseDir False bearerToken
  forkIO $ updater ioData

    -- Start server
  putStrLn $ "starting on localhost:" ++ show srvPort
  runServer ioData Nothing Nothing (enumerateTree coneEntrySetId 1 myTree)

updater :: IOData AccessToken -> IO ()
updater ioData = getCustom ioData >>= go
  where
    go bearerToken = do
      threadDelay $ 10 * 1000 * 1000

      putStrLn "Refreshing Twitter data..."
      rt <- retrieveTrending bearerToken :: IO (Maybe String)
      let res = case rt of
                  Nothing -> "[]"
                  (Just a) -> drop 1 . reverse . drop 1 $ reverse a
      writeFile "trending.json" res

      let model' = enumerateTree coneEntrySetId 1 (buildTwitCone . extractTrending $ B.pack res)

      applyIOSetter ioData model' setTestModel

      go bearerToken
