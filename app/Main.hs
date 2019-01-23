{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Simple
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as B
import qualified Data.Aeson as J
import Types

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
