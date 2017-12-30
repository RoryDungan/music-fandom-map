{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Lib
import Web.Scotty
import Network.Wai.Middleware.Static
import Data.Aeson (FromJSON, ToJSON)

main :: IO ()
main = scotty 3000 $ do
    get "/test" $
        text "Hello world"

    middleware $ staticPolicy (noDots >-> addBase "frontend")