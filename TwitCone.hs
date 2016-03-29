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
import Data.Time.Clock
import Data.Time.Format
import qualified System.IO.Strict         as S

import Control.Concurrent                 (threadDelay, forkIO)
import Control.DeepSeq

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
    c <- S.readFile (fname ++ ".json")
    return . decode $ B.pack c

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

getConeEntryFromString :: Text -> ConeEntry
getConeEntryFromString s = ConeEntry {
    ceEntryId       = 0,
    ceLabel         = s,
    ceTargetUri     = Nothing,
    ceComment       = Nothing,
    ceIconName      = Nothing,
    ceStlName       = Nothing,
    ceColor         = Nothing,
    ceIsLeaf        = True,
    ceTextId        = s
}

trendingQuery :: Maybe [Trending] -> [(Text, [SearchQuery])]
trendingQuery Nothing = []
trendingQuery (Just ts) = fmap (\t -> (Ts.as_of t, Ts.trends t)) ts

entriesFromTrending :: Maybe [Trending] -> [(ConeEntry, [ConeEntry])]
entriesFromTrending mts = fmap (\(s, qs) -> ((getConeEntryFromString s), (fmap getConeEntryFromQuery qs))) $ trendingQuery mts

node :: [ConeTree] -> ConeEntry -> ConeTree
node [] e = RoseLeaf e {ceIsLeaf = ceIsLeaf e && True, ceTextId = "tId_" `append` ceLabel e} (-1) []
node cs e = RoseLeaf e {ceIsLeaf = False, ceTextId = "tId_" `append` ceLabel e} (-1) cs

leaf = node []

buildTwitCone :: [(ConeEntry, [ConeEntry])] -> ConeTree
buildTwitCone ts =
    RoseLeaf emptyLeaf {ceIsLeaf = False, ceTextId = "tId_root", ceLabel = domainLabel} (-1) $
        fmap (\(a, b) -> (node (fmap leaf b) a)) ts

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
            threadDelay $ 5 * 60 * 1000 * 1000

            mts <- retrieveTrending bearerToken
            case mts of
                Nothing   -> putStrLn "Error receiving new trending topics"
                (Just ts) -> do
                    let model' = prepTree . buildTwitCone $ entriesFromTrending mts
                    applyIOSetter ioData model' setTestModel
                    writeFile "/Users/work/code/TwitCone/trending.json" (B.unpack $ encodePretty ts)
                    t <- getCurrentTime
                    putStrLn $ "Twitter data refreshed at " ++ (formatTime defaultTimeLocale "%c" t)

            go bearerToken
