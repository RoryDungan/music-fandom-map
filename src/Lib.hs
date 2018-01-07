{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( ArtistEntry (Artist)
    , artistName
    , countryValues
    , ArtistName
    , CountryTitle
    , StreamsPct
    , Track(Track)
    , trackName
    , trackArtistName
    , trackStreams
    , CountryEntry(Country)
    , countryTitle
    , countryArtistName
    , streamsPct
    ) where

-- text
import Data.Text (Text, pack, unpack)

-- bson & bson-mapping
import Data.Bson
import Data.Bson.Mapping

-- containers
import qualified Data.Map as Map

-- aeson
import Data.Aeson (ToJSON, toJSON, object, (.=))

-- cassava
import Data.Csv
    ( FromNamedRecord(parseNamedRecord)
    , (.:)
    )

type ArtistName = String
type Streams = Int
type StreamsPct = Float
type TrackName = String
type CountryTitle = Text

data Track = Track 
    { trackName :: TrackName
    , trackArtistName :: ArtistName
    , trackStreams :: Streams
    } deriving (Show)

instance FromNamedRecord Track where
    parseNamedRecord r =
        Track
            <$> r .: "Track Name"
            <*> r .: "Artist"
            <*> r .: "Streams"

data CountryEntry = Country 
    { countryTitle :: CountryTitle
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


data ArtistEntry = Artist 
    { artistName :: ArtistName
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
                c =: s 
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
        cast' (value f) >>= (\s -> return (label f, s))
    ))
