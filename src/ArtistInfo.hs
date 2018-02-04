-- Tools for getting descriptions and images for artists from Last.fm
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ArtistInfo 
    ( ArtistSummary
    , name 
    , image 
    , bio 
    , ArtistImage
    , imageUrl 
    , size 
    , ArtistBio
    , summary 
    , decodeArtistInfo
    ) where 

import GHC.Generics
import Control.Applicative (optional)
import Control.Monad
 
import Data.Aeson (FromJSON, Value, eitherDecode, parseJSON, withObject, (.:))
import Data.Aeson.Types (Parser, parseEither)

import Data.Text (Text)

import Data.ByteString.Lazy (ByteString)

import Data.Vector (Vector)

data ArtistSummary = ArtistSummary 
    { name :: Text
    , image :: Vector ArtistImage
    , bio :: ArtistBio
    } deriving (Show, Generic)
instance FromJSON ArtistSummary where


data ArtistImage = ArtistImage
    { imageUrl :: Text
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

decodeArtistInfo :: ByteString -> Either String ArtistSummary
decodeArtistInfo b = 
    case eitherDecode b of 
        Left e  -> Left e 
        Right v -> join $ parseEither parseArtistInfo v

parseArtistInfo :: Value -> Parser (Either String ArtistSummary)
parseArtistInfo = withObject "tuple" $ \obj -> do 
    artist <- optional (obj .: "artist")
    err    <- optional (obj .: "message")

    return $ case artist of
        Just a  -> Right a
        Nothing -> case err of 
            Just e  -> Left e
            Nothing -> Left "Error parsing artist"