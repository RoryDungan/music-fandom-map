{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( ArtistEntry (Artist)
    , artistName
    , countryValues
    , ArtistName
    , CountryTitle
    , StreamsPct
    , decodeItems
    , processData
    , artistSummaries
    ) where

-- base
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Data.Function
import Data.List (sort, sortBy, groupBy, foldl', foldl1')

-- text
import Data.Text (pack, unpack)

-- bson & bson-mapping
import Data.Bson
import Data.Bson.Mapping

-- containers
import qualified Data.Map as Map

-- aeson
import Data.Aeson (ToJSON, toJSON, object, (.=))

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- cassava
import Data.Csv
  ( FromNamedRecord(parseNamedRecord)
  , (.:)
  )
import qualified Data.Csv as Cassava

-- vector
import Data.Vector (Vector)

type ArtistName = String
type Streams = Int
type StreamsPct = Float
type TrackName = String
type CountryTitle = String

data Track = Track { trackName :: TrackName
                   , trackArtistName :: ArtistName
                   , trackStreams :: Streams
                   } deriving (Show)

instance FromNamedRecord Track where
    parseNamedRecord r =
        Track
            <$> r .: "Track Name"
            <*> r .: "Artist"
            <*> r .: "Streams"

data CountryEntry = Country { countryTitle :: CountryTitle
                            , countryArtistName :: ArtistName
                            , streamsPct :: StreamsPct
                            } deriving (Show)

instance Eq CountryEntry where
    (Country n1 a1 s1) == (Country n2 a2 s2) =
        n1 == n2 && a1 == a2 && s1 == s2

instance Ord CountryEntry where
    (Country n1 a1 s1) `compare` (Country n2 a2 s2) =
        let nComp = n1 `compare` n2
            aComp = a1 `compare` a2
            sComp = s1 `compare` s2
        in  if nComp /= EQ then nComp else
            if aComp /= EQ then aComp else sComp


data ArtistEntry = Artist { artistName :: ArtistName
                          , countryValues :: [(CountryTitle, StreamsPct)]
                          } deriving (Show, Eq)
                          
instance ToJSON ArtistEntry where
    toJSON (Artist name streams) = object 
        [
            "name" .= name, 
            "streams" .= Map.fromList streams
        ]

instance Bson ArtistEntry where
    toBson a = [
        "artistName" =: artistName a, 
        "streams" =: map (\(c,s) -> 
                (pack c) =: s 
            ) (countryValues a)
        ]

    fromBson document = do
        name <- look "artistName" document >>= cast
    
        case maybeStreams of 
            Nothing -> fail "Could not read field"
            Just streams -> return (Artist name streams)
        where maybeStreams = look "streams" document 
                >>= cast'List >>= mapStreams


mapStreams :: [Field] -> Maybe [(CountryTitle, StreamsPct)]
mapStreams = sequence . (map (\f -> 
        cast' (value f) >>= (\s -> return (unpack (label f), s))
    ))

processData :: CountryTitle -> [Track] -> [CountryEntry]
processData c xs =
    let countryTotal = fromIntegral $
            foldl' (\acc t -> acc + trackStreams t) 0 xs

    in  map (\(Track _ a s) -> Country c a ((fromIntegral s) / countryTotal)) xs
    & sort
    & groupBy (\(Country n1 a1 _) (Country n2 a2 _) -> n1 == n2 && a1 == a2)
    & map (foldl1' (\(Country n a p1) (Country _ _ p2) -> Country n a (p1 + p2)))

artistSummaries :: [CountryEntry] -> [ArtistEntry]
artistSummaries xs =
    sortBy (\(Country _ a1 _) (Country _ a2 _) -> a1 `compare` a2) xs
    & groupBy (\(Country _ a1 _) (Country _ a2 _) -> a1 == a2)
    & map (\xs' -> 
        let name = 
                countryArtistName (head xs')
            streamsPerCountry = 
                map (\(Country t _ s) -> (t,s)) xs'

        in  Artist name streamsPerCountry
    )

decodeItems :: ByteString -> Either String (Vector Track)
decodeItems = fmap snd . Cassava.decodeByName

-- testing
catchShowIO :: IO a -> IO (Either String a)
catchShowIO action =
    fmap Right action `Exception.catch` handleIOException
    where
        handleIOException :: IOException -> IO (Either String a)
        handleIOException =
            return . Left . show

decodeItemsFromFile :: FilePath -> IO (Either String (Vector Track))
decodeItemsFromFile filePath =
    either Left decodeItems <$>
        catchShowIO (ByteString.readFile filePath)

-- To test:
-- *Main Lib> import qualified Data.Vector as V
-- *Main Lib V> items <- decodeItemsFromFile "au.csv"
-- *Main Lib V> fmap ((processData "AU") . V.toList) items
