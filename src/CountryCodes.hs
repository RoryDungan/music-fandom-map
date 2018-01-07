{-# LANGUAGE OverloadedStrings #-}
module CountryCodes
    ( CountryInfo
    , alpha2ToAlpha3
    ) where

import Data.Csv
import Data.Text (Text)
import Data.Vector (Vector, find)

data CountryInfo = CountryInfo
        { alpha2 :: Text
        , alpha3 :: Text
        } deriving (Show)

-- enable loading from CSV of countries
instance FromNamedRecord CountryInfo where
    parseNamedRecord r = 
        CountryInfo 
            <$> r .: "alpha-2"
            <*> r .: "alpha-3"

-- Takes a 2 digit country code and a vector of codes to look up
-- and returns the 3 digit equivilant.
alpha2ToAlpha3 :: Text -> Vector CountryInfo -> Maybe Text
alpha2ToAlpha3 alpha2Value =
    fmap alpha3 . find (\c -> alpha2 c == alpha2Value) 

