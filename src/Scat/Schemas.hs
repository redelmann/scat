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
    , paranoiac

    -- * PIN
    , pin

    -- * Pass phrases
    , pokemons
    , diceware

    -- * Pattern lock
    , androidPatternLock
    ) where

import Data.List (intercalate, (\\))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Monoid
import Control.Monad (replicateM)
import System.IO

import Scat.Builder

import Paths_scat

-- | Password builder.
type Schema = Builder String

-- | Paranoiac mode, entropy of 512 bits.
paranoiac :: Schema
paranoiac = replicateM 78 ascii

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


-- | Generates an Android lock pattern, of specified length.
androidPatternLock :: Int -> Schema
androidPatternLock number = do
    xs <- loop (min number (height * width)) []
    return $ intercalate " - " $ map showPosition xs
  where
    -- Gets `n` points.
    loop :: Int -> [(Int, Int)] -> Builder [(Int, Int)]
    loop n xs | n <= 0 = return $ reverse xs
    loop n xs = do
        x <- oneOf $ possibilities xs
        loop (n - 1) (x : xs)

    -- Grid dimensions.
    height = 3
    width = 3

    -- Text representation for a position.
    showPosition (1, 1) = "center"
    showPosition (i, j) = vshow i ++ hshow j
      where
        vshow 0 = "north"
        vshow 1 = ""
        vshow _ = "south"

        hshow 0 = "west"
        hshow 1 = ""
        hshow _ = "east"

    -- All positions.
    allPositions = [(i, j) | i <- [0 .. height - 1], j <- [0 .. width - 1]]

    {- Possible positions given a list of already used ones.
       The head of the list is the last used position. -}
    possibilities [] = allPositions
    possibilities pps@(p : ps) = filter isPossible candidates
      where
        candidates = allPositions \\ pps

        isPossible q = all (`elem` ps) $ interfere p q

    -- The list of positions that are on the way between two positions.
    interfere (i, j) (k, l) = do
        r <- [1 .. steps - 1]
        return (i + r * vstep, j + r * hstep)
      where
        vdiff = k - i
        hdiff = l - j

        steps = gcd vdiff hdiff

        vstep = vdiff `div` steps
        hstep = hdiff `div` steps

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
