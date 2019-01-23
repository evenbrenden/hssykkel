{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Aeson.Types

data AvailabilityStation = AvailabilityStation {
    availabilityStationId  :: Int,
    bikes           :: Int,
    locks           :: Int
} deriving Show

instance ToJSON AvailabilityStation where
  toJSON x = object [
    "id" .= availabilityStationId x,
    "bikes" .= locks x,
    "locks" .= bikes x ]

instance FromJSON AvailabilityStation where
  parseJSON = withObject "availabilityStation" $ \o -> do
    availabilityStationId <- o .: "id"
    availability <- o .: "availability"
    bikes <- availability .: "bikes"
    locks <- availability .: "locks"
    return AvailabilityStation{..}

data TitleStation = TitleStation {
    titleStationId :: Int,
    title :: String,
    subtitle :: String
} deriving Show

instance ToJSON TitleStation where
  toJSON x = object [
    "id" .= titleStationId x,
    "title" .= title x,
    "subtitle" .= subtitle x ]

instance FromJSON TitleStation where
  parseJSON = withObject "titleStation" $ \o -> do
    titleStationId <- o .: "id"
    title <- o .: "title"
    subtitle <- o .: "subtitle"
    return TitleStation{..}
