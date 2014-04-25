{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.ByteString (ByteString)
import System.Exit (exitFailure)

import Scat

scatterTest :: String -> ByteString -> ByteString -> ByteString -> Integer -> IO ()
scatterTest name service password code expected = do
    putStrLn $ "Testing " ++ name
    when (scatter service password code /= expected) exitFailure

main :: IO ()
main = do
    -- Mainly here for regression testing.
    scatterTest "scatter example 1"
        "github" "pony1234" "AGDE2-DGXA4-33DLQ-WEDAP-GYPQ9"
        7273969660509039708598560774226985084748596416599584268407087707065680457723095089909612048211188783512827089141818809294015697773231266655764062992251214
    scatterTest "scatter example 2"
        "facebook" "pony1234" "AGDE2-DGXA4-33DLQ-WEDAP-GYPQ9"
        13262460002149113723264840055577914239548551696334299511162926224707398066055935936276077079458293092086000192365956813128644769941942691534958728554307440

    -- TODO: Test also the schemas.
