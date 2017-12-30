{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Lib
import Web.Scotty
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Data.Monoid

main :: IO ()
main = scotty 3000 $ do
    get "/" $
        text "Hello world"