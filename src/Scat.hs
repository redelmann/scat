{-# LANGUAGE OverloadedStrings, PatternGuards #-}

-- | Password scatterer.
module Main (main) where

import Data.Monoid
import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString.Char8 as C
import System.IO
import System.Exit
import System.Console.ANSI
import Control.Exception
import Control.Monad.Reader
import Crypto.Scrypt

import Scat.Schemas
import Scat.Builder
import Scat.Options

-- | Generates the seed integer given a service, a password and a code.
scatter :: ByteString -> ByteString -> ByteString -> Integer
scatter k pw c = foldr (\ n s -> fromIntegral n + 256 * s) 0 $
    unpack $ getHash $ scrypt params (Salt k) (Pass $ pw <> c)
  where
    Just params = scryptParams 14 8 50

-- | Main type of the program.
type Scat a = ReaderT Options IO a

-- | Input visibility.
data Visibility = Shown | Hidden | Erased

-- | Should the input be echoed?
shouldShow :: Visibility -> Bool
shouldShow Shown  = True
shouldShow Hidden = False
shouldShow Erased = True

-- | Should the input be erased afterwards?
shouldErase :: Visibility -> Bool
shouldErase Shown  = False
shouldErase Hidden = False
shouldErase Erased = True

{- | Generates a password, given a input password,
     a service name (category, website, etc.),
     a code, and a password `Schema`.

     The parameters are specified as command line arguments.
     The password can be read from @stdin@ if not already provided. -}
main :: IO ()
main = getOptions >>= runReaderT scat

-- | Main program.
scat :: Scat ()
scat = do
    s  <- getSchema
    k  <- getService
    pw <- getPassword
    c  <- getCode
    printVerbose "Generated password:\n"
    ms <- fmap size ask
    showGenerated $ evalBuilder (getBuilder s ms) $ scatter k pw c

-- | Prints out the generated password.
showGenerated :: String -> Scat ()
showGenerated gen = do
    v <- fmap verbose ask
    a <- fmap ansi ask
    let ok = v && a
    liftIO $ do
        when ok $ setSGR [SetSwapForegroundBackground True]
        putStrLn gen
        when ok $ setSGR [SetSwapForegroundBackground False]


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
    getPass = prompt Hidden "Password: "

    getPassConfirm = do
        a <- prompt Hidden "Password: "
        b <- prompt Hidden "Confirm: "
        if a == b
            then return a
            else do
                printVerbose "Passwords do not match, please retry.\n"
                getPassConfirm

-- | Ask a for input on the command line, with the specified prompt.
prompt :: Visibility -> String -> Scat ByteString
prompt vis str = do
    printVerbose str
    old <- liftIO $ hGetEcho stdin
    pw <- liftIO $ bracket_
        (hSetEcho stdin $ shouldShow vis)
        (hSetEcho stdin old)
        C.getLine
    v <- fmap verbose ask
    a <- fmap ansi ask
    when (shouldErase vis && a && v) $ liftIO $ do
        cursorUpLine 1
        cursorForward $ length str
        clearFromCursorToScreenEnd
        cursorDownLine 1
    unless (shouldShow vis) $ printVerbose "\n"
    return pw

-- | Gets the service.
getService :: Scat ByteString
getService = do
    mk <- fmap service ask
    case mk of
        Just k -> return $ C.pack k
        Nothing -> prompt Shown "Service: "

-- | Gets the code.
getCode :: Scat ByteString
getCode = do
    uc <- fmap useCode ask
    if uc
        then do
            mc <- fmap code ask
            case mc of
                Just st -> return $ C.pack st
                Nothing -> prompt Erased "Code: "
        else return ""


-- | Lists all the available schemas.
schemas :: [(String, Scat Schema)]
schemas =
    [ ("safe", return safe)
    , ("alpha", return alphanumeric)
    , ("parano", return paranoiac)
    , ("pin", return pin)
    , ("lock", return androidPatternLock)
    , ("diceware", liftIO diceware)
    , ("pokemons", liftIO pokemons) ]

-- | Gets the schema to generate the new password.
getSchema :: Scat Schema
getSchema = do
    name <- fmap schema ask
    case lookup name schemas of
        Just s -> s
        Nothing -> liftIO $ do
            hPutStrLn stderr "Error: Unknown schema"
            exitFailure
