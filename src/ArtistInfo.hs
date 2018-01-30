-- Tools for getting descriptions and images for artists from Last.fm
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ArtistInfo () where 

import GHC.Generics
import Control.Applicative (optional)
 
import Data.Aeson (FromJSON, Value, parseJSON, withObject, (.:))
import Data.Aeson.Types (Parser)

import Data.Text (Text)
import qualified Data.Text as T

import Data.Vector (Vector)

data ArtistSummary = ArtistSummary 
    { name :: Text
    , image :: Vector ArtistImage
    , bio :: ArtistBio
    } deriving (Show, Generic)
instance FromJSON ArtistSummary where


data ArtistImage = ArtistImage
    { url :: Text
    , size :: Text 
    } deriving (Show)

instance FromJSON ArtistImage where
    parseJSON = withObject "ArtistImage" $ \v -> ArtistImage 
        <$> v .: "#text"
        <*> v .: "size"

data ArtistBio = ArtistBio 
    { summary :: Text 
    } deriving (Show, Generic)
instance FromJSON ArtistBio where

parseArtistInfo :: Value -> Parser (Either String ArtistSummary)
parseArtistInfo = withObject "tuple" $ \obj -> do 
    artist <- optional (obj .: "artist")
    err    <- optional (obj .: "error")

    return $ case artist of
        Just a  -> Right a
        Nothing -> case err of 
            Just e  -> Left e
            Nothing -> Left "Error parsing artist"