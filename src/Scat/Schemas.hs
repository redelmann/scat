{-# LANGUAGE BangPatterns #-}

{- | This module defines `Schema`s,
     which can generate passwords. -}
module Scat.Schemas
    (
    -- * Type
      Schema

    -- ** Constructors
    , withDefaultSize
    , ignoreSize

    -- ** Destructor
    , getBuilder

    -- * Built-in schemas
    -- ** Passwords
    , safe
    , alphanumeric
    , paranoiac

    -- ** PIN
    , pin

    -- ** Pass phrases
    , pokemons
    , diceware

    -- ** Pattern lock
    , androidPatternLock
    ) where

import Data.Ratio ((%))
import Data.List (intercalate, (\\))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Monoid
import Control.Monad (replicateM)
import System.IO

import Scat.Builder

import Paths_scat

-- | Password builder.
data Schema = Schema
    { defaultSize :: Int
    , builder     :: Int -> Builder String }

-- | Returns a `Builder` given an optional size.
getBuilder :: Schema -> Maybe Int -> Builder String
getBuilder schema Nothing  = builder schema $ defaultSize schema
getBuilder schema (Just s) = builder schema s

-- | Specifies the Schema will not be sensible to any size parameter.
ignoreSize :: Builder String -> Schema
ignoreSize = Schema undefined . const

-- | Specifies the Schema accepts a size parameter.
withDefaultSize :: Int -> (Int -> Builder String) -> Schema
withDefaultSize = Schema

-- | Paranoiac mode, entropy of 512 bits with the default size of 78.
paranoiac :: Schema
paranoiac = withDefaultSize 78 $ \ s -> replicateM s ascii

{- | Generates a password,
     containing upper case letters,
     lower case letters,
     digits and symbols.
     Entropy of about 115 bits for length 18. -}
safe :: Schema
safe = withDefaultSize 18 $ \ s -> do
    let number = max s 4
        lBound = max 1 $ floor $ number % 8
        uBound = ceiling $ number % 4
    nUpper <- inRange (lBound, uBound)
    nDigit <- inRange (lBound, uBound)
    nSpecial <- inRange (lBound, uBound)
    let nLower = number - nUpper - nSpecial - nDigit
    uppers <- replicateM nUpper upper
    digits <- replicateM nDigit digit
    specials <- replicateM nSpecial special
    lowers <- replicateM nLower lower
    shuffle (uppers <> digits <> specials <> lowers)

{- | Generates a password,
     containing upper case letters,
     lower case letters and
     digits, but no symbols.
     Entropy of about 104.2 bits for length 18. -}
alphanumeric :: Schema
alphanumeric = withDefaultSize 18 $ \ s -> do
    let number = max s 4
        lBound = max 1 $ floor $ number % 8
        uBound = ceiling $ number % 4
    nUpper <- inRange (lBound, uBound)
    nDigit <- inRange (lBound, uBound)
    let nLower = number - nUpper - nDigit
    uppers <- replicateM nUpper upper
    digits <- replicateM nDigit digit
    lowers <- replicateM nLower lower
    shuffle (uppers <> digits <> lowers)

{- | Generates a PIN number, of length `n`.
     Entropy of about @3.32 * n@ bits. -}
pin :: Schema
pin = withDefaultSize 6 $ \ s -> replicateM s digit

-- | Generates an Android lock pattern.
androidPatternLock :: Schema
androidPatternLock = withDefaultSize 9 $ \ s -> do
    xs <- loop (min s (height * width)) []
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

{- | Generates a password with `s` of the original Pokemons and their level.
     Entropy of about 55.5 bits for 4 pokemons. -}
pokemons :: IO Schema
pokemons = fromFile "pokemons.txt" $ \ vect -> 
    withDefaultSize 4 $ \ s -> do
        ps <- replicateM s $ oneOfV vect
        ls <- replicateM s $ inRange (1, 100 :: Int)
        let ss = zipWith (\ p l -> p ++ " " ++ show l) ps ls
        return $ intercalate ", " ss

{- | Generates a password with `s` words
     from the Diceware list.
     Entropy of about 64.6 bits for 5 words. -}
diceware :: IO Schema
diceware = fromFile "diceware.txt" $ \ vect ->
    withDefaultSize 5 $ \ s -> do
        ws <- replicateM s $ oneOfV vect
        return $ unwords ws

-- | Feeds all lines of a file to a function and gets the result.
fromFile :: FilePath -> (Vector String -> a) -> IO a
fromFile fp bs = do
    fp' <- getDataFileName fp
    withFile fp' ReadMode $ \ h -> do
        !vect <- fmap (V.fromList . lines) $ hGetContents h
        return $ bs vect
