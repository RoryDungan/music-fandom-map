{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

-- base
import Data.Map (Map)
import qualified Data.Map as Map

-- Scotty
import Web.Scotty
import Network.Wai.Middleware.Static
import Network.HTTP.Types (status500)

-- Aeson
import Data.Aeson (ToJSON, toJSON, object, (.=))

-- BSON
import Data.Bson (ObjectId(), look, cast')

-- MongoDB
import Database.MongoDB (Action, Document, Pipe, Query, master, connect, host, 
                         access, close, find, select, project, rest, (=:))
import Database.MongoDB.Query (Collection)

-- text
import qualified Data.Text as T

-- DB collection with the data processed by the data wrangler in it.
statsCollection :: Collection
statsCollection = "stats"

-- Information associating artist names and their Object IDs. Used when the 
-- /artsts route is requested.
data ArtistInfo = ArtistInfo ObjectId String deriving (Show) 

main :: IO ()
main = do 
    pipe <- connect (host "localhost")

    scotty 3000 $ do

        get "/api/v1/artists" $ do
            resBson <- allArtists pipe
            case sequence (map artistInfoFromBson resBson) of
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


        middleware $ staticPolicy (noDots >-> addBase "frontend")

    close pipe

runQuery :: Pipe -> Query -> ActionM [Document]
runQuery pipe query = access pipe master "music-map" (find query >>= rest)

allArtists :: Pipe -> ActionM [Document]
allArtists pipe = 
    runQuery pipe (select [] statsCollection) { project = ["artistName" =: 1] }

artistStats :: Pipe -> ObjectId -> ActionM [Document]
artistStats pipe oid = 
    runQuery pipe (select ["_id" =: oid] statsCollection)

artistInfoFromBson :: Document -> Maybe ArtistInfo
artistInfoFromBson document = do 
    oid <- look "_id" document >>= cast'
    name <- look "artistName" document >>= cast'
    return (ArtistInfo oid name)