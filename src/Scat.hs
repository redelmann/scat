{-# LANGUAGE OverloadedStrings, PatternGuards #-}

-- | Password scatterer.
module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString (unpack)
import qualified Data.ByteString.Char8 as C
import System.IO
import System.Exit
import Control.Exception
import Control.Monad.Reader
import Crypto.Scrypt

import Scat.Schemas
import Scat.Builder
import Scat.Options

-- | Generates the seed integer given a key and a password.
scatter :: ByteString -> ByteString -> Integer
scatter k pw = foldr (\ c s -> fromIntegral c + 256 * s) 0 $
        unpack $ unHash $ scrypt params (Salt k) (Pass pw)
    where
        Just params = scryptParams 14 8 50

-- | Main type of the program.
type Scat a = ReaderT Options IO a

{- | Generates a password, given a input password,
     a key (category, website, etc.),
     and a password `Schema`.

     The parameters are specified as command line arguments.
     The password can be read from @stdin@ if not already provided. -}
main :: IO ()
main = getOptions >>= runReaderT scat

-- | Main program.
scat :: Scat ()
scat = do
    k  <- getKey
    s  <- getSchema
    pw <- getPassword
    printVerbose "Generated password:\n"
    liftIO $ putStrLn $ evalBuilder s $ scatter k pw

-- | Prints, if the verbosity level allows it.
printVerbose :: String -> Scat ()
printVerbose str = do
    v <- fmap verbose ask
    when v $ liftIO $ do
        putStr str
        hFlush stdout

-- | Gets the password.
getPassword :: Scat ByteString
getPassword = do
    mpw <- fmap password ask
    case mpw of
        -- Ask for the password on stdin.
        Nothing -> do
            c <- fmap confirm ask
            if c
                then getPassConfirm
                else getPass

        -- Retrieve the password from the arguments.
        Just st -> return $ C.pack st
  where
    getPass = askPassword "Password: "

    getPassConfirm = do
        a <- askPassword "Password: "
        b <- askPassword "Confirm: "
        if a == b
            then return a
            else do
                printVerbose "Passwords do not match, please retry.\n"
                getPassConfirm

-- | Ask a password on the command line, with the specified prompt.
askPassword :: String -> Scat C.ByteString
askPassword str = do
    printVerbose str
    old <- liftIO $ hGetEcho stdin
    pw <- liftIO $ bracket_
        (hSetEcho stdin False)
        (hSetEcho stdin old)
        C.getLine
    printVerbose "\n"
    return pw

-- | Gets the key.
getKey :: Scat ByteString
getKey = fmap (C.pack . key) ask

-- | Gets the schema to generate the new password.
getSchema :: Scat Schema
getSchema = do
    name <- fmap schema ask
    case name of
        -- Safe, the default.
        "safe"  -> return safe

        -- Alphanumeric.
        "alpha" -> return alphanumeric

        -- Paranoiac
        "parano" -> return paranoiac

        -- PIN.
        'p' : 'i' : 'n' : xs | [(n, "")] <- reads xs -> return $ pin n

        -- Passphrase using Diceware's list.
        "diceware" -> liftIO diceware

        -- Passphrase using Pokemons.
        "pokemons" -> liftIO pokemons

        -- Unkown.
        _ -> liftIO $ do
            hPutStrLn stderr "Error: Unknown schema"
            exitFailure
