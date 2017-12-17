{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.Wreq
import Control.Lens
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
    let url = "http://httpbin.org/get"
    putStrLn $ "GET " ++ url
    r <- get url
    C.putStrLn (r ^. responseBody)
