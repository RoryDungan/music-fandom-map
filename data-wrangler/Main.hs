{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL

countries = ["au.csv", "ad.html"]
-- countries = ["us", "gb", "ad", "ar", "at", "au", "be", "bg", "bo", "br", "ca",
--              "ch", "cl", "co", "cr", "cy", "cz", "de", "dk", "do", "ec", "ee",
--              "es", "fi", "fr", "gr", "gt", "hk", "hn", "hu", "id", "ie", "is",
--              "it", "jp", "lt", "lu", "lv", "mc", "mt", "mx", "my", "ni", "nl",
--              "no", "nz", "pa", "pe", "ph", "pl", "pt", "py", "se", "sg", "sk",
--              "sv", "th", "tr", "tw", "uy"]


main :: IO ()
main = do
    -- let url = "https://spotifycharts.com/regional/"
    --     suffix = "/daily/latest/download"
    let url = "http://localhost:3001/"
        suffix = ""
    let urls = map (\c -> url ++ c ++ suffix) countries
    reqs <- mapM get urls
    mapM_ (\r -> C.putStrLn (r ^. responseHeader "Content-Type")) reqs
