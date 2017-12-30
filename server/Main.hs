{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Static
--import Data.Aeson (FromJSON, ToJSON)
import Data.Bson (ObjectId())
import Database.MongoDB (Action, Document, Pipe, Query, master, connect, host, 
                         access, close, find, select, project, rest, (=:))
import Database.MongoDB.Query (Collection)
import qualified Data.Text.Lazy as T

statsCollection :: Collection
statsCollection = "stats"

main :: IO ()
main = do 
    pipe <- connect (host "localhost")

    scotty 3000 $ do

        get "/artists" $ do
            res <- allArtists pipe
            text $ T.pack $ show res

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