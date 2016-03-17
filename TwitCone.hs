{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import ConeServer.RunServer
import ConeServer.ConeTypes
import ConeServer.Types                   (RoseTree(..), enumerateTree)
import ConeServer.Utils

import Network.OAuth.OAuth2.Internal      (AccessToken)
import Network.Wai.Handler.Warp           (Port)

import Data.Aeson
import Data.ByteString.Lazy.Char8         as B (readFile, ByteString, pack, unpack)
import Data.Text                          as T (Text, pack, append)
import Data.Aeson.Encode.Pretty           (encodePretty)

import Control.Concurrent                 (threadDelay, forkIO)

import Types                              as Ts
import TwitterConnector

baseDir :: FilePath
baseDir = "/Users/work/code/ConeServer"

srvPort :: Port
srvPort = 8080

twitterSearchURL :: Text
twitterSearchURL = "https://ritetag.com/hashtag-stats/"

domainLabel :: Text
domainLabel = "Trending search queries"

readJSON :: String -> IO (Maybe [Ts.Trending])
readJSON fname = do
  f <- B.readFile $ fname ++ ".json"
  return $ decode f

getConeEntryFromQuery :: Ts.SearchQuery -> ConeEntry
getConeEntryFromQuery sq = ConeEntry {
  ceEntryId       = 0,
  ceLabel         = Ts.name sq,
  ceTargetUri     = Just $ Ts.url sq,
  ceComment       = Nothing,
  ceIconName      = Nothing,
  ceStlName       = Nothing,
  ceColor         = Nothing,
  ceIsLeaf        = True,
  ceTextId        = Ts.name sq
}

getQs :: Maybe [Trending] -> [SearchQuery]
getQs Nothing = []
getQs (Just (t:_)) = Ts.trends t

entriesFromTrending :: Maybe [Trending] -> [ConeEntry]
entriesFromTrending mts = fmap getConeEntryFromQuery $ getQs mts

node :: ConeEntry -> [ConeTree] -> ConeTree
node e [] = RoseLeaf e {ceIsLeaf = ceIsLeaf e && True, ceTextId = "tId_" `append` ceLabel e} (-1) []
node e cs = RoseLeaf e {ceIsLeaf = False, ceTextId = "tId_" `append` ceLabel e} (-1) cs

buildTwitCone :: [ConeEntry] -> ConeTree
buildTwitCone ts =
  RoseLeaf emptyLeaf {ceIsLeaf = False, ceTextId = "tId_root", ceLabel = domainLabel} (-1) $
    fmap (flip node []) ts

prepTree :: ConeTree -> ConeTree
prepTree c = enumerateTree coneEntrySetId 1 c

-- refreshModel :: IO AccessToken ->

main = do
  putStrLn "Constructing TwitCone"
  j <- readJSON "trending"
  let myTree = buildTwitCone $ entriesFromTrending j

  -- Prepare server and fork updateJob
  bearerToken <- requestToken
  ioData <- initServer srvPort baseDir False bearerToken
  forkIO $ updater ioData

    -- Start server
  putStrLn $ "starting on localhost:" ++ show srvPort
  runServer ioData Nothing Nothing (prepTree myTree)

updater :: IOData AccessToken -> IO ()
updater ioData = getCustom ioData >>= go
  where
    go bearerToken = do
      threadDelay $ 10 * 1000 * 1000

      putStrLn "Refreshing Twitter data..."
      mts <- retrieveTrending bearerToken
      case mts of
        Nothing   -> putStrLn "Error receiving new trending topics"
        (Just ts) -> do
          let model' = prepTree . buildTwitCone $ entriesFromTrending mts
          applyIOSetter ioData model' setTestModel
          writeFile "/Users/work/code/TwitCone/trending_.json" (B.unpack $ encodePretty ts)

      go bearerToken
