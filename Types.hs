{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}

module Types where

import Control.Applicative              (optional)
import Data.Aeson
import GHC.Generics
import qualified Data.Text              as T

-- Hashtag models a generic Hashtag as may be contained in responses from
-- various endpoints of the RiteTag API

data Hashtag = Hashtag {
  tag :: T.Text,
  tweets :: Int,
  retweets :: Int,
  potential_views :: Int,
  links :: Float,
  photos :: Maybe Float,
  mentions :: Float,
  color :: Int
} deriving (Generic, Show)

instance ToJSON Hashtag where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Hashtag where
  parseJSON = withObject "Hashtag" $ \o -> do
    tag <- o .: "tag"
    tweets <- o .: "tweets"
    retweets <- o .: "retweets"
    potential_views <- o .: "potential_views"
    links <- o .: "links"
    photos <- optional (o .: "photos")
    mentions <- o .: "mentions"
    color <- o .: "color"
    return Hashtag{..}

-- TrendingHashtags models a list of trending hashtags as returned by the
-- RiteTag API.

data TrendingHashtags = TrendingHashtags {
  thResult :: Bool,
  thMessage :: T.Text,
  thCode :: Int,
  thTags :: [Hashtag]
} deriving (Generic, Show)

instance ToJSON TrendingHashtags where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TrendingHashtags where
  parseJSON = withObject "TrendingHashtags" $ \o -> do
    thResult <- o .: "result"
    thMessage <- o .: "message"
    thCode <- o .: "code"
    thTags <- o .: "tags"
    return TrendingHashtags{..}

-- HashtagStats models an array of statistics as returned by the RiteTag API

data HashtagStats = HashtagStats {
  hsResult :: Bool,
  hsCode :: Int,
  hsHashtag :: T.Text,
  hsMessage :: T.Text,
  hsAssociatedHashtags :: [Hashtag]
} deriving (Generic, Show)

instance ToJSON HashtagStats where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON HashtagStats where
  parseJSON = withObject "HashtagStats" $ \o -> do
    hsResult <- o .: "result"
    hsCode <- o .: "code"
    hsHashtag <- o .: "hashtag"
    hsMessage <- o .: "message"
    hsAssociatedHashtags <- o .: "associatedHashtags"
    return HashtagStats{..}
