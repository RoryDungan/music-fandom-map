{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Lib

-- base
import Data.Function
import Data.Maybe

-- containers
import qualified Data.Map as Map

-- Scotty
import Web.Scotty
import Network.Wai.Middleware.Static
import Network.HTTP.Types (status400, status404, status500)

-- Aeson
import Data.Aeson (toJSON)

-- BSON & bson-mapping
import Data.Bson (ObjectId(), look, cast, cast', cast'List)
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
data ArtistInfo = ArtistInfo 
    { artist_id :: ObjectId
    , artistName :: Text
    , artistStreams :: [ArtistStats]
    , artistDescription :: Maybe Text 
    , imageUrl :: Maybe Text 
    } deriving (Show, Eq)

instance Bson ArtistInfo where
    fromBson document = do
        oid <- look "_id" document >>= cast
        name <- look "artistName" document >>= cast
        
        let maybeStreams = look "streams" document
                >>= cast'List >>= readArtistStatsFromField

        let description = look "description" document >>= cast' 
            image = look "imageUrl" document >>= cast'
        
        case maybeStreams of
            Nothing -> 
                fail "Could not read 'streams' field."
            Just streams -> 
                return (ArtistInfo oid name streams description image)

    toBson (ArtistInfo oid name streams description image) = catMaybes 
        [ ("_id" =:) <$> Just oid
        , ("artistName" =:) <$> Just name
        , ("streams" =:) <$> Just (map (\(ArtistStats c s) ->
                c =: s
            ) streams)
        , ("description" =:) <$> description 
        , ("imageUrl" =:) <$> image
        ]

main :: IO ()
main = do
    pipe <- connect (host "localhost")

    scotty 3000 $ do

        get "/api/v1/artists" $ do
            resBson <- allArtists pipe
            case sequence (map fromBson resBson) of
                Just res -> json $ res
                    -- filter to artists that appear in 2 or more countries
                    & filter (\a -> length (artistStreams a) > 2)
                    & map (\(ArtistInfo o n _ _ _) -> (T.pack (show o), n))
                    & Map.fromList
                    & toJSON

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
                                json $ toJSON res

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
    runQuery pipe (select [] statsCollection)

artistStats :: Pipe -> ObjectId -> ActionM [Document]
artistStats pipe oid =
    runQuery pipe (select ["_id" =: oid] statsCollection)
