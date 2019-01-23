{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import Data.Aeson.Types

data AvailabilityStation = AvailabilityStation {
    availabilityId  :: Int,
    bikes           :: Int,
    locks           :: Int
} deriving Show

instance ToJSON AvailabilityStation where
  toJSON x = object [
    "id"    .= availabilityId x,
    "bikes" .= locks x,
    "locks" .= bikes x ]

instance FromJSON AvailabilityStation where
  parseJSON = withObject "availabilityStation" $ \o -> do
    availabilityId <- o .: "id"
    availability <- o .: "availability"
    bikes <- availability .: "bikes"
    locks <- availability .: "locks"
    return AvailabilityStation{..}

data AvailabilityStations = AvailabilityStations {
    availabilityStations :: [AvailabilityStation]
} deriving Show

instance ToJSON AvailabilityStations where
  toJSON x = object [ "stations" .= availabilityStations x ]

instance FromJSON AvailabilityStations where
  parseJSON = withObject "availabilityStations" $ \o -> do
    availabilityStations <- o .: "stations"
    return AvailabilityStations{..}

data TitleStation = TitleStation {
    titleId     :: Int,
    title       :: String,
    subtitle    :: String
} deriving Show

instance ToJSON TitleStation where
  toJSON x = object [
    "id"        .= titleId x,
    "title"     .= title x,
    "subtitle"  .= subtitle x ]

instance FromJSON TitleStation where
  parseJSON = withObject "titleStation" $ \o -> do
    titleId <- o .: "id"
    title <- o .: "title"
    subtitle <- o .: "subtitle"
    return TitleStation{..}

data TitleStations = TitleStations {
    titleStations :: [TitleStation]
} deriving Show

instance ToJSON TitleStations where
  toJSON x = object [ "stations" .= titleStations x ]

instance FromJSON TitleStations where
  parseJSON = withObject "titleStations" $ \o -> do
    titleStations <- o .: "stations"
    return TitleStations{..}
