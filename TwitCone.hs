{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import ConeServer.RunServer
import ConeServer.ConeTypes
import ConeServer.Types                         (RoseTree(..), enumerateTree)
import ConeServer.Utils

import Network.OAuth.OAuth2.Internal            (AccessToken)
import Network.Wai.Handler.Warp                 (Port)

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8     as B (readFile, ByteString, pack, unpack)
import Data.Text                                (Text, pack, unpack, append)
import qualified Data.Text.IO                   as TIO (putStrLn)
import Data.Aeson.Encode.Pretty                 (encodePretty)
import Data.Time.Clock
import Data.Time.Format
import qualified System.IO.Strict               as S

import Control.Concurrent                       (threadDelay, forkIO)
import Control.DeepSeq

import Types                                    as Ts
import Web.Twitter.Types                        (SearchStatus(..), searchResultStatuses, User(..))
import TwitterConnector

-- base directory for cone server
baseDir :: FilePath
baseDir = "/Users/work/code/ConeServer"

-- base directory for TwitCone
tcBaseDir :: FilePath
tcBaseDir = "/Users/work/code/TwitCone/"

-- cone server port
srvPort :: Port
srvPort = 8080

-- How often is new data added (in seconds)
updateInterval :: Int
updateInterval = 1 * 60

twitterSearchURL :: Text
twitterSearchURL = "https://ritetag.com/hashtag-stats/"

-- show this in upper left hand corner of conecanvas
domainLabel :: Text
domainLabel = "Trending search queries"


-- decode trending terms from json serialization
readJSON :: String -> IO (Maybe [Ts.Trending])
readJSON fname = do
    c <- S.readFile (tcBaseDir ++ fname ++ ".json")
    return . decode $ B.pack c

-- decode stored trends from json and add new trending instance
appendNewTrends :: Trending -> IO [Trending]
appendNewTrends t = do
    mts_old <- readJSON "trending"
    let ts = case mts_old of
                Nothing -> [t]
                (Just ts_old) -> [t] ++ ts_old
    return ts



entryFromText :: Text -> ConeEntry
entryFromText s = ConeEntry {
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

entryFromQuery :: Ts.SearchQuery -> ConeEntry
entryFromQuery sq = ConeEntry {
    ceEntryId       = 0,
    ceLabel         = Ts.name sq,
    ceTargetUri     = Just $ Ts.url sq,
    ceComment       = Nothing,
    ceIconName      = Nothing,
    ceStlName       = Nothing,
    ceColor         = Nothing,
    ceIsLeaf        = False,
    ceTextId        = Ts.name sq
}

entryFromSearchStatus :: SearchStatus -> ConeEntry
entryFromSearchStatus s = ConeEntry {
    ceEntryId       = 0,
    ceLabel         = userName $ searchStatusUser s,
    ceTargetUri     = Nothing,
    ceComment       = Nothing,
    ceIconName      = Nothing,
    ceStlName       = Nothing,
    ceColor         = Nothing,
    ceIsLeaf        = True,
    ceTextId        = pack . show $ searchStatusId s
}


-- Utilities for building ConeTrees
node :: [ConeTree] -> ConeEntry -> ConeTree
node [] e = RoseLeaf e {ceIsLeaf = ceIsLeaf e && True, ceTextId = "tId_" `append` ceLabel e} (-1) []
node cs e = RoseLeaf e {ceIsLeaf = False, ceTextId = "tId_" `append` ceLabel e} (-1) cs

leafNode :: ConeEntry -> ConeTree
leafNode = node []

rootNode :: [ConeTree] -> ConeTree
rootNode = RoseLeaf
    emptyLeaf {ceIsLeaf = False, ceTextId = "tId_root", ceLabel = domainLabel}
    (-1)

prepTree :: ConeTree -> ConeTree
prepTree c = enumerateTree coneEntrySetId 1 c


-- Methods for constructing ConeTree from stored data

-- add statuses to a leaf node representing a search query
statusTree :: Maybe [SearchStatus] -> ConeEntry -> ConeTree
statusTree Nothing tr = leafNode tr
statusTree (Just ss) tr = node (fmap (leafNode . entryFromSearchStatus) ss) tr

-- add trending search queries to a leaf node representing a "trending entry"
queryTree :: [SearchQuery] -> ConeEntry -> ConeTree
queryTree [] trEntry = leafNode trEntry
queryTree sqs trEntry = node
    (fmap
        (\sq->statusTree (tweets sq) (entryFromQuery sq))
        sqs)
    trEntry

-- build base tree containing subtress with trending data
trendingTree :: Maybe [Trending] -> ConeTree
trendingTree Nothing = rootNode []
trendingTree (Just ts) = rootNode $
    fmap (\t->queryTree (trends t) (entryFromText (Ts.as_of t)))
    ts


main = do
    putStrLn "Loading last TwitCone from disk.."
    j <- readJSON "trending"
    let myTree = trendingTree j

    -- Prepare server and fork updateJob
    bearerToken <- requestToken
    ioData <- initServer srvPort baseDir False bearerToken
    forkIO $ updater ioData

    -- Start server
    putStrLn $ "Starting on localhost:" ++ show srvPort
    runServer ioData Nothing Nothing (prepTree myTree)


updater :: IOData AccessToken -> IO ()
updater ioData = getCustom ioData >>= go
    where
        go bearerToken = do
            threadDelay $ updateInterval * 1000 * 1000

            putStrLn "Update started..."
            mts <- retrieveTrending bearerToken
            case mts of
                Nothing   -> putStrLn "Error receiving new trending topics"
                (Just ts) -> do
                    trendWithStatuses <- addStatuses bearerToken $ head ts
                    ts' <- appendNewTrends trendWithStatuses

                    writeFile "/Users/work/code/TwitCone/trending.json" (B.unpack $ encodePretty ts')

                    let newmodel = prepTree $ trendingTree (Just ts')
                    applyIOSetter ioData newmodel setTestModel

                    t <- getCurrentTime
                    putStrLn $ "Twitter data refreshed at " ++ (formatTime defaultTimeLocale "%c" t)

            go bearerToken
