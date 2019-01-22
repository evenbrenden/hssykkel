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
    "bikes" .= bikes p,
    "locks"  .= locks p ]

instance FromJSON Availability where
  parseJSON (Object v) =
    Availability <$> v .: "bikes"
                 <*> v .: "locks"
  parseJSON v = typeMismatch "Availability" v

data Station = Station {
    id :: Int,
    availability :: Availability
} deriving Show

instance ToJSON Station where
  toJSON p = object [
    "id" .= Main.id p,
    "availability"  .= availability p ]

instance FromJSON Station where
  parseJSON (Object v) =
    Station <$> v .: "id"
            <*> v .: "availability"
  parseJSON v = typeMismatch "Station" v

data Stations = Stations {
    stations :: [Station]
} deriving Show

instance ToJSON Stations where
  toJSON p = object [
    "stations" .= stations p ]

instance FromJSON Stations where
  parseJSON (Object v) =
    Stations <$> v .: "stations"
  parseJSON v = typeMismatch "Stations" v

decode :: L.ByteString -> Maybe Stations
decode = J.decode

-- "{\"stations\":[{\"id\":177,\"availability\":{\"bikes\":0,\"locks\":29,\"overflow_capacity\":false}}],\"updated_at\":\"2019-01-22T13:08:29+00:00\",\"refresh_rate\":10.0}"

main :: IO ()
main = do
    tokentxt <- liftIO $ readFile "token.txt"
    let token = B.pack . head . words $ tokentxt
    let request =
            setRequestHeader "Client-Identifier" [token]
            $ "GET http://oslobysykkel.no/api/v1/stations/availability"
    response <- httpLBS request
    let stations = fromJust $ Main.decode (getResponseBody response)
    putStrLn $ show stations
