{-# LANGUAGE OverloadedStrings #-}

module TwitterConnector where

import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.Internal            (AccessToken)
import Network.HTTP.Conduit
import System.Posix.Env                         (getEnv)
import qualified Data.ByteString.Lazy.Char8     as BL (putStrLn, ByteString(..))
import qualified Data.ByteString.Char8          as B (pack)

import Data.Aeson.Types                         (FromJSON)
import qualified Data.Text                      as T (pack, unpack, append)
import qualified Data.Text.IO                   as TIO (putStrLn)
import Web.Twitter.Types                        (SearchResult(..), SearchMetadata(..), SearchStatus(..))

import Types
import Web.Twitter.Types                        (SearchStatus(..), searchResultStatuses, User(..))




-- How many trending topics to store
trendingCount :: Int
trendingCount = 10

-- How many statuses to store per trending item
statusCount :: Int
statusCount = 10

-- Yahoo! Where On Earth ID of Germany
-- Find available IDs in available_countries.json (as of March 2016)
berlinID :: String
berlinID = "23424829"




twitterKey :: IO OAuth2
twitterKey = do
    env <- getEnv "TWITTER_CLIENT_SECRET"
    let cs = case env of
                Nothing -> error "TWITTER_CLIENT_SECRET not found in environment"
                Just a -> a
    return OAuth2
        { oauthClientId             = "NryOz1RKzhOaBz2QwBd6zuxMK"
        , oauthClientSecret         = B.pack cs
        , oauthOAuthorizeEndpoint   = "https://api.twitter.com/oauth2/token"
        , oauthCallback             = Nothing
        , oauthAccessTokenEndpoint  = ""
        }

placeTrendsURL :: String -> URI
placeTrendsURL placeName = B.pack $ "https://api.twitter.com/1.1/trends/place.json?id=" ++ placeName

searchURL :: String -> URI
searchURL q = B.pack $ "https://api.twitter.com/1.1/search/tweets.json?q=" ++ q

invalidToken :: AccessToken
invalidToken = AccessToken "" Nothing Nothing Nothing

requestToken :: IO AccessToken
requestToken = do
    mgr <- newManager tlsManagerSettings
    tk <- twitterKey
    eResp <- doJSONPostRequest mgr tk (oauthOAuthorizeEndpoint tk) body
    either (\err -> BL.putStrLn err >> return invalidToken) return eResp
    where
        body = [("grant_type", "client_credentials")]



retrieveTrending :: AccessToken -> IO (Maybe [Trending])
retrieveTrending bearerToken = do
    mgr <- newManager tlsManagerSettings
    eResp <- authGetJSON mgr bearerToken (placeTrendsURL berlinID)
    either
        (\err -> BL.putStrLn err >> return Nothing)
        (\resp -> return (Just resp))
        eResp

retrieveTweets :: AccessToken -> String -> IO (Maybe (SearchResult [SearchStatus]))
retrieveTweets bearerToken q = do
    mgr <- newManager tlsManagerSettings
    eResp <- authGetJSON mgr bearerToken (searchURL q)
    either
        (\err -> BL.putStrLn err >> return Nothing)
        (\resp -> return (Just resp))
        eResp


-- add status posts to trending data by calling twitter api
addStatuses :: AccessToken -> Trending -> IO Trending
addStatuses at t = do
    trends' <- mapM (addTweets at) (take statusCount $ trends t)
    return t {trends = trends'}

-- actual twitter api call for addStatuses
addTweets :: AccessToken -> SearchQuery -> IO SearchQuery
addTweets at sq = do
    mtws <- retrieveTweets at (T.unpack (query sq))
    case mtws of
        Nothing -> putStrLn "No result trying to add tweets" >> return sq
        Just tws -> TIO.putStrLn (T.pack "Loaded tweets for " `T.append` (query sq))
            >> return sq {tweets = Just $ take trendingCount (searchResultStatuses tws)}
