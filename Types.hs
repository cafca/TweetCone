{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}

module Types where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import qualified Data.Text              as T
import Web.Twitter.Types                (SearchStatus(..))

-- Trending models a list of trending hashtags as modeled by the
-- Twitter Trending Place API
-- https://dev.twitter.com/rest/reference/get/trends/place

data Trending = Trending {
    created_at :: T.Text,
    as_of :: T.Text,
    trends :: [SearchQuery]
} deriving (Generic, Show)

instance FromJSON Trending
instance ToJSON Trending where
    toEncoding = genericToEncoding defaultOptions

-- SearchQuery models individual trending topics that may be
-- contained in the result from the Twitter trending API call

data SearchQuery = SearchQuery {
    name :: T.Text,
    url :: T.Text,
    query :: T.Text,
    tweet_volume :: Maybe Int,
    tweets :: Maybe [SearchStatus]
} deriving (Generic, Show)

instance FromJSON SearchQuery
instance ToJSON SearchQuery where
    toEncoding = genericToEncoding defaultOptions
