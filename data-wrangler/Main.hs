{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

-- base
import Data.Either
import Control.Monad.Trans (liftIO)

-- bytestring
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL

-- wreq
import Network.Wreq

--lens
import Control.Lens

countries :: [String]
countries = ["au.csv", "ad.html"]
-- countries = ["us", "gb", "ad", "ar", "at", "au", "be", "bg", "bo", "br", "ca",
--              "ch", "cl", "co", "cr", "cy", "cz", "de", "dk", "do", "ec", "ee",
--              "es", "fi", "fr", "gr", "gt", "hk", "hn", "hu", "id", "ie", "is",
--              "it", "jp", "lt", "lu", "lv", "mc", "mt", "mx", "my", "ni", "nl",
--              "no", "nz", "pa", "pe", "ph", "pl", "pt", "py", "se", "sg", "sk",
--              "sv", "th", "tr", "tw", "uy"]

-- Only keep responses that were actually CSVs
filterCSVs :: [(t, Response body)] -> [(t, Response body)]
filterCSVs responses = 
    filter (\(_,r) -> "data/csv" `C.isInfixOf` (contentTypeHeader r)) responses
        where contentTypeHeader r = r ^. responseHeader "Content-Type"

-- Takes a country code and retuns a tuple with the same 
-- country name and the result of the request.
getForCountry :: String -> IO (String, Response CL.ByteString)
getForCountry c = do 
    -- let url = "https://spotifycharts.com/regional/"
    --     suffix = "/daily/latest/download"
    let url = "http://localhost:3001/"
        suffix = ""

    res <- get (url ++ c ++ suffix)
    return (c,res)

main :: IO ()
main = do

    reqs <- mapM getForCountry countries
    let csvs = filterCSVs reqs

    --liftEither :: (a, Either b c) -> Either b (a, c)
    let liftEither (c,e) = case e of 
            Left msg  -> Left msg
            Right res -> Right (c,res)

    let tracks = map liftEither $ 
            map (\(c,r) -> (c, decodeItems (r ^. responseBody))) csvs

    let track1 = head $ rights tracks

    
    return ()
