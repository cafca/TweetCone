{-# LANGUAGE OverloadedStrings #-}

module TwitterConnector where

import Network.OAuth.OAuth2
import Network.HTTP.Conduit
import System.Posix.Env                       (getEnv)
import qualified Data.ByteString.Lazy.Char8   as BL (putStrLn)
import qualified Data.ByteString.Char8   as B (pack)


twitterKey :: Maybe String -> OAuth2
twitterKey Nothing = error "TWITTER_CLIENT_SECRET not found in environment"
twitterKey (Just cs) = OAuth2
  { oauthClientId             = "NryOz1RKzhOaBz2QwBd6zuxMK"
  , oauthClientSecret         = B.pack cs
  , oauthOAuthorizeEndpoint   = "https://api.twitter.com/oauth2/token"
  , oauthCallback             = Nothing
  , oauthAccessTokenEndpoint  = ""
  }

invalidToken :: AccessToken
invalidToken = AccessToken "" Nothing Nothing Nothing

requestToken :: IO AccessToken
requestToken = do
  tcs <- getEnv "TWITTER_CLIENT_SECRET"
  let tk = twitterKey tcs
  mgr <- newManager tlsManagerSettings
  eResp <- doJSONPostRequest mgr tk (oauthOAuthorizeEndpoint tk) body
  either (\err -> BL.putStrLn err >> return invalidToken) return eResp
  where
    body = [("grant_type", "client_credentials")]
