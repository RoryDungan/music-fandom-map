{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( ArtistName
    , Streams
    , TrackName
    , TrackEntry
    , CountryEntry
    ) where

import Data.List (sort, groupBy, foldl1')
import Data.Csv
import Control.Applicative

type ArtistName = String
type Streams = Int
type TrackName = String
type CountryTitle = String

data TrackEntry = Track TrackName ArtistName Streams deriving (Show)

instance FromNamedRecord TrackEntry where
    parseNamedRecord r =
        Track <$> r .: "Track Name" <*> r .: "Artist" <*> r .: "Streams"

data CountryEntry = Country CountryTitle ArtistName Streams deriving (Show)

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

processData :: [(TrackEntry,CountryTitle)] -> [CountryEntry]
processData =
    map (foldl1' (\(Country n a p1) (Country _ _ p2) -> Country n a (p1 + p2)))
    . groupBy (\(Country n1 a1 _) (Country n2 a2 _) -> n1 == n2 && a1 == a2)
    . sort
    . map (\(Track title artist streams,c) -> Country c artist streams)
