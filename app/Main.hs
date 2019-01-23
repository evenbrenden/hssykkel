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

decodeAvailabilities :: L.ByteString -> Maybe AvailabilityStations
decodeAvailabilities = J.decode

decodeTitles :: L.ByteString -> Maybe TitleStations
decodeTitles = J.decode

comprehend :: AvailabilityStations -> TitleStations -> [(AvailabilityStation, TitleStation)]
comprehend as ts = [(x, y) | x <- availabilityStations as, y <- titleStations ts, availabilityId x == titleId y]

toString :: (AvailabilityStation, TitleStation) -> String
toString (x, y) = title y ++ " " ++ subtitle y ++ " has " ++ show (bikes . availability $ x) ++  " available bikes and " ++ show (locks . availability $ x) ++ " available locks."

main :: IO ()
main = do
    tokentxt <- liftIO $ readFile "token.txt"
    let token = B.pack . head . words $ tokentxt
    let availabilitiesRequest =
            setRequestHeader "Client-Identifier" [token]
            $ "GET http://oslobysykkel.no/api/v1/stations/availability"
    availabilitiesResponse <- httpLBS availabilitiesRequest
    let availabilitiesStations = fromJust $ Main.decodeAvailabilities (getResponseBody availabilitiesResponse)
    let titlesRequest =
            setRequestHeader "Client-Identifier" [token]
            $ "GET http://oslobysykkel.no/api/v1/stations"
    titlesResponse <- httpLBS titlesRequest
    let titleStations = fromJust $ Main.decodeTitles (getResponseBody titlesResponse)
    let comprehended = comprehend availabilitiesStations titleStations
    putStrLn $ unlines (map toString comprehended)
