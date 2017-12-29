{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Lib

-- base
import Data.Either
import Control.Monad.Trans (liftIO)
import qualified Data.Vector as V

-- bytestring
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL

-- wreq
import Network.Wreq

--lens
import Control.Lens

-- MongoDB
import Database.MongoDB     (Action, Document, Value, access, close, connect, 
                             delete, exclude, find, host, insertMany, master,
                             project, rest, select, sort, (=:))

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
    filter (\(_,r) -> "text/csv" `C.isInfixOf` (contentTypeHeader r)) responses
        where contentTypeHeader r = r ^. responseHeader "Content-Type"

-- Takes a country code and retuns a tuple with the same 
-- country name and the result of the request.
getForCountry :: String -> IO (String, Response CL.ByteString)
getForCountry c = do 
    -- let url = "https://spotifycharts.com/regional/"
    --     suffix = "/daily/latest/download"
    let baseUrl = "http://localhost:3001/"
        suffix = ""
        url = (baseUrl ++ c ++ suffix)

    res <- get url
    return (c,res)

-- Insert the specified list of tracks into the database
insertEntries :: [CountryEntry] -> Action IO [Value]
insertEntries countryEntries = insertMany "stats" bsonData
    where bsonData = map (\c -> 
            [
                "name" =: countryTitle c, 
                "artistName" =: artistName c, 
                "streams" =: streams c
            ])
            countryEntries

main :: IO ()
main = do
    -- Request the CSVs for all countries
    reqs <- mapM getForCountry countries
    putStrLn "Got URLs:"
    mapM_ (\r -> C.putStrLn (r ^. responseHeader "Content-Type")) (map snd reqs) 
    putStrLn ""

    let csvs = filterCSVs reqs

    putStrLn "Filtered:"
    mapM_ putStrLn (map fst csvs)

    --liftEither :: (a, Either b c) -> Either b (a, c)
    let liftEither (c,e) = case e of 
            Left msg  -> Left msg
            Right res -> Right (c,res)

    let trackEntries = rights $ map liftEither $ 
            map (\(c,r) -> (c, decodeItems (r ^. responseBody))) csvs

    let countryEntries = 
            (fmap (\(c,v) -> ((processData c) . V.toList) v) trackEntries) 
            >>= id

    putStrLn "entries: "
    mapM_ (putStrLn . show) countryEntries

    -- pipe <- connect (host "127.0.0.1")
    -- e <- access pipe master "music-map" (run countryEntries)
    -- close pipe

    -- print e


run :: [CountryEntry] -> Action IO ()
run tracks = do
    return ()
