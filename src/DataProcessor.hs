module DataProcessor
    ( processData
    , artistSummaries
    , decodeItems
    ) where

import Lib

-- base
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Data.Function
import Data.List (sort, sortBy, groupBy, foldl', foldl1')

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import qualified Data.Csv as Cassava

-- vector
import Data.Vector (Vector)

{-| 
  Takes a country code and, a list of track entires (track name, artist name, 
  and number of streams), and returns a list of country entries (mapping of a 
  country code, artist name, and the percentage of streams in the top 200 songs
  for that country that the artist has).
-}
processData :: CountryTitle -> [Track] -> [CountryEntry]
processData c xs =
    let countryTotal = fromIntegral $
            foldl' (\acc t -> acc + trackStreams t) 0 xs

    in  map (\(Track _ a s) -> Country c a ((fromIntegral s) / countryTotal)) xs
    & sort
    & groupBy (\(Country n1 a1 _) (Country n2 a2 _) -> n1 == n2 && a1 == a2)
    & map (foldl1' (\(Country n a p1) (Country _ _ p2) -> Country n a (p1 + p2)))

{-|
  Takes a list of country entries and groups entries for the same artist together
  so that we get an object with the artist name and a list of their streams
  for each country. 
-}
artistSummaries :: [CountryEntry] -> [(ArtistName, [ArtistStats])]
artistSummaries xs =
    sortBy (\(Country _ a1 _) (Country _ a2 _) -> a1 `compare` a2) xs
    & groupBy (\(Country _ a1 _) (Country _ a2 _) -> a1 == a2)
    & map (\xs' ->
        let name =
                countryArtistName (head xs')
            streamsPerCountry =
                map (\(Country t _ s) -> ArtistStats t s) xs'

        in  (name, streamsPerCountry)
    )

-- |Read top 200 track charts from CSV data
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
