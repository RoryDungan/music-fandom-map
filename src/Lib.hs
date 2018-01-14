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
    , ArtistStats(ArtistStats)
    ) where

-- text
import Data.Text (Text)

-- bson & bson-mapping
import Data.Bson
import Data.Bson.Mapping

-- aeson
import Data.Aeson (ToJSON, toJSON, object, (.=))

-- cassava
import Data.Csv
    ( FromNamedRecord(parseNamedRecord)
    , (.:)
    )

type ArtistName = Text
type Streams = Int
type StreamsPct = Float
type TrackName = Text
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
    , countryValues :: [ArtistStats]
    } deriving (Show, Eq)

instance ToJSON ArtistEntry where
    toJSON (Artist name streams) = object
        [
            "name" .= name,
            "streams" .= streams
        ]

instance Bson ArtistEntry where
    toBson a = [
        "artistName" =: artistName a,
        "streams" =: map (\(ArtistStats c s) ->
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



data ArtistStats = ArtistStats CountryTitle StreamsPct deriving (Show, Eq)

instance ToJSON ArtistStats where
    toJSON (ArtistStats c s) = object
        [
            "countryCode" .= c,
            "streams" .= s
        ]


mapStreams :: [Field] -> Maybe [ArtistStats]
mapStreams = sequence
    . (map (\f ->
        cast' (value f) >>= (\s -> return (ArtistStats (label f) s))
    ))
