{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( ArtistName
    , Streams
    , TrackName
    , TrackEntry
    , CountryEntry
    , decodeItemsFromFile
    , decodeItems
    , processData
    ) where

-- base
import Control.Applicative
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- list
import Data.List (sort, sortBy, groupBy, foldl1')

-- cassava
import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector

type ArtistName = String
type Streams = Int
type TrackName = String
type CountryTitle = String

data TrackEntry = Track TrackName ArtistName Streams deriving (Show)

instance FromNamedRecord TrackEntry where
    parseNamedRecord r =
        Track
            <$> r .: "Track Name"
            <*> r .: "Artist"
            <*> r .: "Streams"

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

processData :: CountryTitle -> [TrackEntry] -> [CountryEntry]
processData c =
    map (foldl1' (\(Country n a p1) (Country _ _ p2) -> Country n a (p1 + p2)))
    . groupBy (\(Country n1 a1 _) (Country n2 a2 _) -> n1 == n2 && a1 == a2)
    . sort
    . map (\(Track _ artist streams) -> Country c artist streams)

decodeItems :: ByteString -> Either String (Vector TrackEntry)
decodeItems = fmap snd . Cassava.decodeByName

-- testing
catchShowIO :: IO a -> IO (Either String a)
catchShowIO action =
    fmap Right action `Exception.catch` handleIOException
    where
        handleIOException :: IOException -> IO (Either String a)
        handleIOException =
            return . Left . show

decodeItemsFromFile :: FilePath -> IO (Either String (Vector TrackEntry))
decodeItemsFromFile filePath =
    either Left decodeItems <$>
        catchShowIO (ByteString.readFile filePath)

-- To test:
-- *Main Lib> import qualified Data.Vector as V
-- *Main Lib V> items <- decodeItemsFromFile "au.csv"
-- *Main Lib V> fmap ((processData "AU") . V.toList) items
