{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Lib

-- base
import GHC.Generics

-- containers
import qualified Data.Map as Map

-- Scotty
import Web.Scotty
import Network.Wai.Middleware.Static
import Network.HTTP.Types (status400, status404, status500)

-- Aeson
import Data.Aeson (toJSON)

-- Cassava
import Data.Csv

-- BSON & bson-mapping
import Data.Bson (ObjectId(), look, cast)
import Data.Bson.Mapping

-- MongoDB
import Database.MongoDB (Document, Pipe, Query, master, connect, host,
                         access, close, find, select, project, rest, (=:))
import Database.MongoDB.Query (Collection)

-- text
import Data.Text (Text)
import qualified Data.Text as T

-- DB collection with the data processed by the data wrangler in it.
statsCollection :: Collection
statsCollection = "stats"

-- Information associating artist names and their Object IDs. Used when the
-- /artsts route is requested.
data ArtistInfo = ArtistInfo ObjectId String deriving (Show, Eq)

instance Bson ArtistInfo where
    fromBson document = do
        oid <- look "_id" document >>= cast
        name <- look "artistName" document >>= cast
        return (ArtistInfo oid name)

    toBson (ArtistInfo oid name) = [
            "_id" =: oid,
            "artistName" =: name
        ]

data ArtistStats = ArtistStats
    { countryCode :: Text
    , streams :: Float
    } deriving (Show, Generic)

instance FromNamedRecord ArtistStats
instance ToNamedRecord ArtistStats
instance DefaultOrdered ArtistStats

main :: IO ()
main = do
    pipe <- connect (host "localhost")

    scotty 3000 $ do

        get "/api/v1/artists" $ do
            resBson <- allArtists pipe
            case sequence (map fromBson resBson) of
                -- Convert our [ArtistInfo] to a `Map String String` for easy
                -- JSON serialisation.
                Just res -> json
                    . toJSON
                    . Map.fromList
                    . map (\(ArtistInfo o n) -> (T.pack (show o), n)) $ res

                Nothing  -> do
                    -- TODO: error logging
                    status status500
                    text "Internal server error"

        get "/api/v1/artist/:id" $ do
            paramId <- param "id"
            case (readEither paramId) of
                Left msg -> do
                    status status400
                    text msg

                Right artistId -> do
                    resBson <- artistStats pipe artistId
                    if length resBson <= 0 then do
                        status status404
                        text "Could not find the specified artist ID"
                    else
                        case fromBson (head resBson) :: Maybe ArtistEntry of
                            Just res -> do
                                let stats = fmap
                                        (\(StreamsForCountry c s) -> ArtistStats c s)
                                        $ countryValues res
                                    csv = encodeDefaultOrderedByName stats

                                setHeader "Content-Type" "text/csv"
                                raw csv

                            Nothing  -> do
                                -- TODO: error logging
                                status status500
                                text "Internal server error"

        get "/" $ do
            setHeader "Content-Type" "text/html"
            file "frontend/dist/index.html"

        middleware
            $ staticPolicy
            $ noDots >-> addBase "frontend/dist"

    close pipe

runQuery :: Pipe -> Query -> ActionM [Document]
runQuery pipe query = access pipe master "music-map" (find query >>= rest)

allArtists :: Pipe -> ActionM [Document]
allArtists pipe =
    runQuery pipe (select [] statsCollection) { project = ["artistName" =: 1] }

artistStats :: Pipe -> ObjectId -> ActionM [Document]
artistStats pipe oid =
    runQuery pipe (select ["_id" =: oid] statsCollection)
