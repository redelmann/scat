{-# LANGUAGE BangPatterns #-}

{- | This module defines `Schema`s,
     which can generate passwords. -}
module Scat.Schemas
    (
    -- * Type
      Schema

    -- * Passwords
    , safe
    , alphanumeric

    -- * PIN
    , pin

    -- * Pass phrases
    , pokemons
    , diceware
    ) where

import Data.List (intercalate)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Monoid
import Control.Monad (replicateM)
import System.IO

import Scat.Builder

import Paths_scat

-- | Password builder.
type Schema = Builder String

{- | Generates a password of length 18,
     containing upper case letters,
     lower case letters,
     digits and symbols.
     Entropy of about 115 bits. -}
safe :: Schema
safe = do
    nUpper <- inRange (2, 5)
    nDigit <- inRange (2, 5)
    nSpecial <- inRange (2, 5)
    let nLower = 18 - nUpper - nSpecial - nDigit
    uppers <- replicateM nUpper upper
    digits <- replicateM nDigit digit
    specials <- replicateM nSpecial special
    lowers <- replicateM nLower lower
    shuffle (uppers <> digits <> specials <> lowers)

{- | Generates a password of length 18,
     containing upper case letters,
     lower case letters and
     digits, but no symbols.
     Entropy of about 104.2 bits. -}
alphanumeric :: Schema
alphanumeric = do
    nUpper <- inRange (2, 5)
    nDigit <- inRange (2, 5)
    let nLower = 18 - nUpper - nDigit
    uppers <- replicateM nUpper upper
    digits <- replicateM nDigit digit
    lowers <- replicateM nLower lower
    shuffle (uppers <> digits <> lowers)

{- | Generates a PIN number, of length `n`.
     Entropy of about @3.32 * n@ bits. -}
pin :: Int -> Schema
pin n = replicateM n digit

{- | Generates a password with 4 of the original Pokemons and their level.
     Entropy of about 55.5 bits. -}
pokemons :: IO Schema
pokemons = fromFile "pokemons.txt" $ \ vect -> do
    ps <- replicateM 4 $ oneOfV vect
    ls <- replicateM 4 $ inRange (1, 100 :: Int)
    let ss = zipWith (\ p l -> p ++ " " ++ show l) ps ls
    return $ intercalate ", " ss

{- | Generates a password with 5 words
     from the Diceware list.
     Entropy of about 64.6 bits. -}
diceware :: IO Schema
diceware = fromFile "diceware.txt" $ \ vect -> do
        ws <- replicateM 5 $ oneOfV vect
        return $ unwords ws

-- | Feeds all lines of a file to a builder.
fromFile :: FilePath -> (Vector String -> Builder a) -> IO (Builder a)
fromFile fp bs = do
    fp' <- getDataFileName fp
    withFile fp' ReadMode $ \ h -> do
        !vect <- fmap (V.fromList . lines) $ hGetContents h
        return $ bs vect
