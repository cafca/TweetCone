{-# LANGUAGE OverloadedStrings #-}

module Config where


import Network.Wai.Handler.Warp                 (Port)
import Data.Text                                (Text)


-- base directory for cone server
baseDir :: FilePath
baseDir = "/Users/work/code/ConeServer"

-- base directory for TweetCone
tcBaseDir :: FilePath
tcBaseDir = "/Users/work/code/TweetCone/"

-- file name of data file
dataFileName :: String
dataFileName = "data/twitter_data.json"

-- cone server port
srvPort :: Port
srvPort = 8080

-- How often is new data added (in seconds)
updateInterval :: Int
updateInterval = 1 * 60

-- show this in upper left hand corner of conecanvas
domainLabel :: Text
domainLabel = "Trending search queries"

-- How many trending topics to store
trendingCount :: Int
trendingCount = 10

-- How many statuses to store per trending item
statusCount :: Int
statusCount = 10

-- This ID specifies which Twitter trending topics to retrieve
-- Yahoo! Where On Earth ID of Germany is 23424829
locationID :: String
locationID = "23424829"
