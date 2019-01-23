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

data TitleStations = TitleStations {
    titleStations :: [TitleStation]
} deriving Show

instance ToJSON TitleStations where
  toJSON x = object [ "stations" .= titleStations x ]

instance FromJSON TitleStations where
  parseJSON = withObject "titleStations" $ \o -> do
    titleStations <- o .: "stations"
    return TitleStations{..}
