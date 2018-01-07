{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import DataProcessor

-- base
import Data.Either

-- vector
import qualified Data.Vector as V

-- bytestring
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL

-- wreq
import Network.Wreq 
    ( Response
    , responseHeader
    , responseBody
    , getWith
    , defaults
    , checkResponse
    )

--lens
import Control.Lens

-- MongoDB
import Database.MongoDB     (Action, Value, access, close, connect, select, 
                             delete, host, insertMany, master)

-- bson-mapping
import Data.Bson.Mapping (toBson)

countries :: [String]
-- countries = ["au.csv", "ad.html"]
countries = ["us", "gb", "ad", "ar", "at", "au", "be", "bg", "bo", "br", "ca",
             "ch", "cl", "co", "cr", "cy", "cz", "de", "dk", "do", "ec", "ee",
             "es", "fi", "fr", "gr", "gt", "hk", "hn", "hu", "id", "ie", "is",
             "it", "jp", "lt", "lu", "lv", "mc", "mt", "mx", "my", "ni", "nl",
             "no", "nz", "pa", "pe", "ph", "pl", "pt", "py", "se", "sg", "sk",
             "sv", "th", "tr", "tw", "uy"]

-- Only keep responses that were actually CSVs
filterCSVs :: [(t, Response body)] -> [(t, Response body)]
filterCSVs responses = 
    filter (\(_,r) -> "text/csv" `C.isInfixOf` (contentTypeHeader r)) responses
        where contentTypeHeader r = r ^. responseHeader "Content-Type"

-- HTTP GET that doesn't throw an exception on non-200 series response.
get :: String -> IO (Response CL.ByteString)
get url = 
    getWith opts url
    where 
        opts = set checkResponse (Just $ \_ _ -> return ()) defaults

-- Takes a country code and retuns a tuple with the same 
-- country name and the result of the request.
getForCountry :: String -> IO (String, Response CL.ByteString)
getForCountry c = do 
    let baseUrl = "https://spotifycharts.com/regional/"
        suffix = "/daily/latest/download"
        url = (baseUrl ++ c ++ suffix)

    res <- get url
    return (c,res)

-- Insert the specified list of tracks into the database
insertEntries :: [ArtistEntry] -> Action IO [Value]
insertEntries artistEntries = insertMany "stats" bsonData
    where bsonData = map toBson artistEntries

clearStats :: Action IO ()
clearStats = delete (select [] "stats")

main :: IO ()
main = do
    -- Request the CSVs for all countries
    putStrLn "Requesting data for specified countries"
    reqs <- mapM getForCountry countries
    putStrLn "Finished downloading data"

    let csvs = filterCSVs reqs

    --liftEither :: (a, Either b c) -> Either b (a, c)
    let liftEither (c,e) = case e of 
            Left msg  -> Left msg
            Right res -> Right (c,res)

    let trackEntries = rights $ map liftEither $ 
            map (\(c,r) -> (c, decodeItems (r ^. responseBody))) csvs

    let artistEntries = artistSummaries $
            (fmap (\(c,v) -> ((processData c) . V.toList) v) trackEntries) >>= id

    putStrLn "Adding data to database"

    pipe <- connect (host "localhost") -- TODO: move this to config
    access pipe master "music-map" $ do 
        clearStats
        insertEntries artistEntries
    close pipe

    putStrLn "Finished inserting data"
