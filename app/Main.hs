{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Simple
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as J
import Data.Aeson.Types

data Availability = Availability {
    bikes :: Int,
    locks :: Int
} deriving Show

instance ToJSON Availability where
  toJSON p = object [
    "bikes"     .= bikes p,
    "locks"     .= locks p ]

instance FromJSON Availability where
  parseJSON (Object v) =
    Availability <$> v .: "bikes"
                 <*> v .: "locks"
  parseJSON v = typeMismatch "Availability" v

data AvailabilityStation = AvailabilityStation {
    availabilityId  :: Int,
    availability    :: Availability
} deriving Show

instance ToJSON AvailabilityStation where
  toJSON p = object [
    "id"            .= availabilityId p,
    "availability"  .= availability p ]

instance FromJSON AvailabilityStation where
  parseJSON (Object v) =
    AvailabilityStation <$> v .: "id"
                        <*> v .: "availability"
  parseJSON v = typeMismatch "AvailabilityStation" v

data AvailabilityStations = AvailabilityStations {
    aStations :: [AvailabilityStation]
} deriving Show

instance ToJSON AvailabilityStations where
  toJSON p = object [
    "stations" .= aStations p ]

instance FromJSON AvailabilityStations where
  parseJSON (Object v) =
    AvailabilityStations <$> v .: "stations"
  parseJSON v = typeMismatch "AvailabilityStations" v

data TitleStation = TitleStation {
    titleId     :: Int,
    title       :: String,
    subtitle    :: String
} deriving Show

instance ToJSON TitleStation where
  toJSON p = object [
    "id"        .= titleId p,
    "title"     .= title p,
    "subtitle"  .= subtitle p ]

instance FromJSON TitleStation where
  parseJSON (Object v) =
    TitleStation <$> v .: "id"
                 <*> v .: "title"
                 <*> v .: "subtitle"
  parseJSON v = typeMismatch "TitleStation" v

data TitleStations = TitleStations {
    titleStations :: [TitleStation]
} deriving Show

instance ToJSON TitleStations where
  toJSON p = object [
    "stations" .= titleStations p ]

instance FromJSON TitleStations where
  parseJSON (Object v) =
    TitleStations <$> v .: "stations"
  parseJSON v = typeMismatch "TitleStations" v

decodeAvailability :: L.ByteString -> Maybe AvailabilityStations
decodeAvailability = J.decode

decodeTitles :: L.ByteString -> Maybe TitleStations
decodeTitles = J.decode

main :: IO ()
main = do
    tokentxt <- liftIO $ readFile "token.txt"
    let token = B.pack . head . words $ tokentxt
    let availabilityRequest =
            setRequestHeader "Client-Identifier" [token]
            $ "GET http://oslobysykkel.no/api/v1/stations/availability"
    availabilityResponse <- httpLBS availabilityRequest
    let availabilityStations = fromJust $ Main.decodeAvailability (getResponseBody availabilityResponse)
    putStrLn $ show availabilityStations
    let titlesRequest =
            setRequestHeader "Client-Identifier" [token]
            $ "GET http://oslobysykkel.no/api/v1/stations"
    titlesResponse <- httpLBS titlesRequest
    let titleStations = fromJust $ Main.decodeTitles (getResponseBody titlesResponse)
    putStrLn $ show titleStations
