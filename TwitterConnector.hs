{-# LANGUAGE OverloadedStrings #-}

module TwitterConnector where

import Network.OAuth.OAuth2
import Network.HTTP.Conduit
import System.Posix.Env                       (getEnv)
import qualified Data.ByteString.Lazy.Char8   as BL (putStrLn, ByteString(..))
import qualified Data.ByteString.Char8        as B (pack)

import Data.Aeson.Types                       (FromJSON)
import Web.Twitter.Types                      (SearchResult(..), SearchMetadata(..))


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

-- Yahoo! Where On Earth ID of Germany
-- Find available IDs in available_countries.json (as of March 2016)
berlinID :: String
berlinID = "23424829"

placeTrendsURL :: String -> URI
placeTrendsURL placeName = B.pack $ "https://api.twitter.com/1.1/trends/place.json?id=" ++ placeName

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

retrieveTrending :: (FromJSON a) => AccessToken -> IO (Maybe a)
retrieveTrending bearerToken = do
  mgr <- newManager tlsManagerSettings
  eResp <- authGetJSON mgr bearerToken (placeTrendsURL berlinID)
  either
    (\err -> BL.putStrLn err >> return Nothing)
    (\resp -> return (Just resp))
    eResp
