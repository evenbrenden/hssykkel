{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson.Types

data Availability = Availability {
    bikes :: Int,
    locks :: Int
} deriving Show

instance ToJSON Availability where
  toJSON x = object [
    "bikes"     .= bikes x,
    "locks"     .= locks x ]

instance FromJSON Availability where
  parseJSON (Object x) =
    Availability <$> x .: "bikes"
                 <*> x .: "locks"
  parseJSON x = typeMismatch "Availability" x

data AvailabilityStation = AvailabilityStation {
    availabilityId  :: Int,
    availability    :: Availability
} deriving Show

instance ToJSON AvailabilityStation where
  toJSON x = object [
    "id"            .= availabilityId x,
    "availability"  .= availability x ]

instance FromJSON AvailabilityStation where
  parseJSON (Object x) =
    AvailabilityStation <$> x .: "id"
                        <*> x .: "availability"
  parseJSON x = typeMismatch "AvailabilityStation" x

data AvailabilityStations = AvailabilityStations {
    availabilityStations :: [AvailabilityStation]
} deriving Show

instance ToJSON AvailabilityStations where
  toJSON x = object [ "stations" .= availabilityStations x ]

instance FromJSON AvailabilityStations where
  parseJSON (Object x) = AvailabilityStations <$> x .: "stations"
  parseJSON x = typeMismatch "AvailabilityStations" x

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
  parseJSON (Object x) =
    TitleStation <$> x .: "id"
                 <*> x .: "title"
                 <*> x .: "subtitle"
  parseJSON x = typeMismatch "TitleStation" x

data TitleStations = TitleStations {
    titleStations :: [TitleStation]
} deriving Show

instance ToJSON TitleStations where
  toJSON x = object [ "stations" .= titleStations x ]

instance FromJSON TitleStations where
  parseJSON (Object x) = TitleStations <$> x .: "stations"
  parseJSON x = typeMismatch "TitleStations" x
