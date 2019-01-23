{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Control.Monad (guard)
import Network.HTTP.Simple
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as J
import Types

decodeAvailabilityStations :: L.ByteString -> Maybe AvailabilityStations
decodeAvailabilityStations = J.decode

decodeTitleStations :: L.ByteString -> Maybe TitleStations
decodeTitleStations = J.decode

comprehend :: AvailabilityStations -> TitleStations -> [(AvailabilityStation, TitleStation)]
comprehend as ts = do
    x <- availabilityStations as
    y <- titleStations ts
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
    let availabilityStations = fromJust $ Main.decodeAvailabilityStations (getResponseBody availabilityStationsResponse)
    let titleStationsRequest =
            setRequestHeader "Client-Identifier" [token]
            $ "GET http://oslobysykkel.no/api/v1/stations"
    titleStationsResponse <- httpLBS titleStationsRequest
    let titleStations = fromJust $ Main.decodeTitleStations (getResponseBody titleStationsResponse)
    let comprehended = comprehend availabilityStations titleStations
    putStrLn $ unlines (map toString comprehended)
