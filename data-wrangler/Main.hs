{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import DataProcessor
import CountryCodes
import ArtistInfo

-- base
import Data.Either
import Control.Monad
import System.IO (hPutStrLn, stderr)

-- text
import Data.Text (Text)
import qualified Data.Text as T

-- vector
import qualified Data.Vector as V

-- bytestring
import qualified Data.ByteString.Lazy as BL
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
    , param
    )

--lens
import Control.Lens

-- vector
import Data.Vector (Vector)

-- Cassava
import Data.Csv (Header, decodeByName)

-- MongoDB
import Database.MongoDB     (Action, Value, access, close, connect, select,
                             delete, host, insertMany, master)

-- bson-mapping
import Data.Bson.Mapping (toBson)

-- ConfigFile
import qualified Data.ConfigFile as Config

countries :: [Text]
-- countries = ["au.csv", "ad.html"]
countries = ["us", "gb", "ad", "ar", "at", "au", "be", "bg", "bo", "br", "ca",
             "ch", "cl", "co", "cr", "cy", "cz", "de", "dk", "do", "ec", "ee",
             "es", "fi", "fr", "gr", "gt", "hk", "hn", "hu", "id", "ie", "is",
             "it", "jp", "lt", "lu", "lv", "mc", "mt", "mx", "my", "ni", "nl",
             "no", "nz", "pa", "pe", "ph", "pl", "pt", "py", "se", "sg", "sk",
             "sv", "th", "tr", "tw", "uy"]

-- |Only keep responses that were actually CSVs
filterCSVs :: [(t, Response body)] -> [(t, Response body)]
filterCSVs responses =
    filter (\(_,r) -> "text/csv" `C.isInfixOf` (contentTypeHeader r)) responses
        where contentTypeHeader r = r ^. responseHeader "Content-Type"

-- |HTTP GET that doesn't throw an exception on non-200 series response.
getUrl :: String -> IO (Response CL.ByteString)
getUrl url =
    getWith opts url
    where
        opts = set checkResponse (Just $ \_ _ -> return ()) defaults

{-|
  Takes a country code and retuns a tuple with the same
  country name and the result of the request.
-}
getStatsForCountry :: Text -> IO (Text, Response CL.ByteString)
getStatsForCountry c = do
    let baseUrl = "https://spotifycharts.com/regional/"
        suffix = "/daily/latest/download"
        url = (baseUrl ++ (T.unpack c) ++ suffix)

    res <- getUrl url
    return (c,res)

{-|
  Takes the name of an artist and a Last.fm API key, looks the artist up on
  Last.fm and retrieves their description and the URL of an image of them
-}
getArtistSummary :: Text -> String -> IO (Either String ArtistSummary)
getArtistSummary artist key = do
    let url = "http://ws.audioscrobbler.com/2.0/"
        opts = defaults & param "method" .~ ["artist.getinfo"]
                        & param "artist" .~ [artist]
                        & param "api_key" .~ [T.pack key]
                        & param "format" .~ ["json"]

    res <- catchShowIO $ getWith opts url

    return $ case res of 
        Right r -> decodeArtistInfo (r ^. responseBody)
        Left e  -> Left e

genArtistEntry :: ArtistName -> [ArtistStats] -> Either String ArtistSummary -> ArtistEntry
genArtistEntry name streams info = 
    let maybeInfo = case info of 
            Left _ -> Nothing
            Right a -> Just a
        description = extractBio <$> maybeInfo
        imageUrl = join $ largeImageURL <$> maybeInfo
        
    in Artist name streams description imageUrl

-- |Insert the specified list of tracks into the database
insertEntries :: [ArtistEntry] -> Action IO [Value]
insertEntries artistEntries = insertMany "stats" bsonData
    where bsonData = map toBson artistEntries

clearStats :: Action IO ()
clearStats = delete (select [] "stats")

map2to3letterCountryCodes :: Monad m => Vector CountryInfo -> (Text, b) -> m (Text, b)
map2to3letterCountryCodes codes (c,r) =
    case alpha2ToAlpha3 c codes of
        Just code -> return (code, r)
        Nothing   -> fail ((T.unpack c) ++ " is not a valid ISO-3166-alpha-2 country code.")

main :: IO ()
main = do
    -- load config
    configFile <- Config.readfile Config.emptyCP "data-wrangler.conf"
    conf <- case configFile of
        Left err -> fail $
            "Error loading config file data-wrangler.conf: " ++ (show err)
        Right c -> return c

    dbHost <- case Config.get conf "DEFAULT" "dbhost" of
        Left _ -> fail "dbhost not specified in data-wrangler.conf"
        Right h -> return h

    lastFmApiKey <- case Config.get conf "DEFAULT" "lastfm_api_key" of 
        Left _ -> fail "lastfm_api_key not specified in data-wrangler.conf"
        Right k -> return k

    -- Get country code info (we'll need this later)
    countryCodesCSV <- BL.readFile "country-codes.csv"
    let countryCodes = case decodeByName countryCodesCSV :: Either String (Header, Vector CountryInfo) of
            Left msg -> fail msg
            Right csv -> snd csv

    -- Request the CSVs for all countries
    putStrLn "Requesting data for specified countries"
    reqs <- mapM getStatsForCountry countries
    putStrLn "Finished downloading data"

    csvs <- mapM (map2to3letterCountryCodes countryCodes) (filterCSVs reqs)

    -- Helper function that takes the either out of the second part of a tuple
    let liftEither (c,e) = case e of
            Left msg  -> Left msg
            Right res -> Right (c,res)

    let trackEntries = rights $ map liftEither $
            map (\(c,r) -> (c, decodeItems (r ^. responseBody))) csvs

    -- Calculate plays per country for each artist
    let statsByArtist = artistSummaries $
            (fmap (\(c,v) -> ((processData c) . V.toList) v) trackEntries) >>= id

    -- Request descriptions and image URLs for artists from Last.fm
    artistEntries <- mapM (\(name, stats) -> do
            putStrLn $ "Getting Last.fm summary for " ++ (show name)

            artistInfo <- getArtistSummary name lastFmApiKey

            -- Log error
            case artistInfo of 
                Left e -> hPutStrLn stderr e
                Right _ -> return ()

            return $ genArtistEntry name stats artistInfo
        ) statsByArtist

    putStrLn "Adding data to database"

    pipe <- connect $ host dbHost
    access pipe master "music-map" $ do
        clearStats
        insertEntries artistEntries
    close pipe

    putStrLn "Finished inserting data"
