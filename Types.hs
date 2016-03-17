{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}

module Types where

import Data.Aeson
import GHC.Generics
import qualified Data.Text              as T

-- Trending models a list of trending hashtags as modeled by the
-- Twitter Trending Place API
-- https://dev.twitter.com/rest/reference/get/trends/place

data Trending = Trending {
  trLocations :: [Location],
  trCreated :: T.Text,
  trAsOf :: T.Text,
  trTrends :: [SearchQuery]
} deriving (Generic, Show)

instance FromJSON Trending where
  parseJSON = withObject "Trending" $ \o -> do
    trLocations <- o .: "locations"
    trCreated <- o .: "created_at"
    trAsOf <- o .: "as_of"
    trTrends <- o .: "trends"
    return Trending{..}

-- The location for which trending items are returned

data Location = Location {
  loName :: T.Text,
  woeid :: Int
} deriving (Generic, Show)

instance FromJSON Location where
  parseJSON = withObject "Location" $ \o -> do
    loName <- o .: "name"
    woeid <- o .: "woeid"
    return Location{..}

-- SearchQuery models individual trending topics that may be
-- contained in the result from the Twitter trending API call

data SearchQuery = SearchQuery {
  sqName :: T.Text,
  sqURI :: T.Text,
  sqQuery :: T.Text,
  sqVolume :: Maybe Int
} deriving (Generic, Show)

instance FromJSON SearchQuery where
  parseJSON = withObject "SearchQuery" $ \o -> do
    sqName <- o .: "name"
    sqURI <- o .: "url"
    sqQuery <- o .: "query"
    sqVolume <- o .:? "tweet_volume"
    return SearchQuery{..}

-- Twitter returns results as a list with one element, which is
-- extracted by this type

data ListNested = ListNested {
  lnbody :: [Trending]
} deriving (Generic, Show)

instance FromJSON ListNested where
  parseJSON = genericParseJSON defaultOptions
