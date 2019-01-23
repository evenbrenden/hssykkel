{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad (guard)
import Network.HTTP.Simple
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as J
import Data.Aeson.Types
import Types

decode :: L.ByteString -> Maybe J.Value
decode = J.decode

parseStations :: FromJSON a => J.Value -> Parser a
parseStations = withObject "stations" $ \o -> o .: "stations"

parseAvailabilityStations :: J.Value -> Parser [AvailabilityStation]
parseAvailabilityStations = parseStations

parseTitleStations :: J.Value -> Parser [TitleStation]
parseTitleStations = parseStations

comprehend :: [AvailabilityStation] -> [TitleStation] -> [(AvailabilityStation, TitleStation)]
comprehend xs ys = do
    x <- xs
    y <- ys
    guard (availabilityStationId x == titleStationId y)
    return (x, y)

toString :: (AvailabilityStation, TitleStation) -> String
toString (x, y) = title y ++ " " ++ subtitle y ++ " has " ++ show (bikes x) ++  " available bikes and " ++ show (locks x) ++ " available locks."

main :: IO ()
main = do
    tokentxt <- liftIO $ readFile "token.txt"
    let token = B.pack . head . words $ tokentxt
    let availabilityStationsRequest =
            setRequestHeader "Client-Identifier" [token]
            $ "GET http://oslobysykkel.no/api/v1/stations/availability"
    availabilityStationsResponse <- httpLBS availabilityStationsRequest
    let decodedAvailabilityStations = fromJust . decode $ getResponseBody availabilityStationsResponse
    let availabilityStations = fromJust $ parseMaybe parseAvailabilityStations decodedAvailabilityStations
    let titleStationsRequest =
            setRequestHeader "Client-Identifier" [token]
            $ "GET http://oslobysykkel.no/api/v1/stations"
    titleStationsResponse <- httpLBS titleStationsRequest
    let decodedTitleStations = fromJust . decode $ getResponseBody titleStationsResponse
    let titleStations = fromJust $ parseMaybe parseTitleStations decodedTitleStations
    let comprehended = comprehend availabilityStations titleStations
    putStrLn $ unlines (Prelude.map toString comprehended)
