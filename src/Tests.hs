{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.ByteString (ByteString)
import System.Exit (exitFailure)

import Scat
import Scat.Builder
import Scat.Schemas

seedGithub :: Integer
seedGithub = 7273969660509039708598560774226985084748596416599584268407087707065680457723095089909612048211188783512827089141818809294015697773231266655764062992251214

seedFacebook :: Integer
seedFacebook = 13262460002149113723264840055577914239548551696334299511162926224707398066055935936276077079458293092086000192365956813128644769941942691534958728554307440

scatterTest :: String -> ByteString -> ByteString -> ByteString -> Integer -> IO ()
scatterTest name service password code expected = do
    putStrLn $ "Testing " ++ name
    when (scatter service password code /= expected) exitFailure

schemaTest :: String -> Schema -> Integer -> String -> IO ()
schemaTest name schema seed expected = do
    putStrLn $ "Testing " ++ name
    when (evalBuilder (getBuilder schema Nothing) seed /= expected) exitFailure

main :: IO ()
main = do
    -- Mainly here for regression testing.
    scatterTest "scatter test 1 - using github as service"
        "github" "pony1234" "AGDE2-DGXA4-33DLQ-WEDAP-GYPQ9"
        seedGithub
    scatterTest "scatter test 2 - using facebook as service"
        "facebook" "pony1234" "AGDE2-DGXA4-33DLQ-WEDAP-GYPQ9"
        seedFacebook

    -- Testing the examples from the README.
    schemaTest "schema test 1 - generating from 'safe' schema, with github seed"
        safe seedGithub
        "k2'8n?QXwmptbJ7D44"

    schemaTest "schema test 2 - generating from 'safe' schema, with facebook seed"
        safe seedFacebook
        "{g6e2hsKjh#Ra*\\ks("

    dicewareSchema <- diceware
    schemaTest "schema test 3 - generating from 'diceware' schema"
        dicewareSchema seedFacebook
        "101 dry whoa foil barb"

    pokemonsSchema <- pokemons
    schemaTest "schema test 4 - generating from 'pokemons' schema"
        pokemonsSchema seedFacebook
        "Snorlax 5, Weedle 35, Raichu 27, Alakazam 99"
