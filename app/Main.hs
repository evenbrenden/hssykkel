{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    let request =
            setRequestHeader "Client-Identifier" [""]
            $ "GET http://oslobysykkel.no/api/v1/stations"
    response <- httpLBS request

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response
