
{- | This modules defines `Builder`s,
     which are simple parsers on `Integer`. -}
module Scat.Builder
    (
    -- * Type
      Builder

    -- * Execution
    , runBuilder
    , evalBuilder
    , execBuilder

    -- * Primitives

    -- ** Numbers
    , lessThan
    , inRange

    -- ** Char
    , digit
    , letter
    , lower
    , upper
    , ascii
    , special

    -- * Combinators
    , useup
    , shuffle
    , oneOf
    , oneOfV
    ) where

import Data.Char (ord, chr)
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Arrow (second)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Scat.Utils.Permutation

-- | Parser acting on an `Integer`.
newtype Builder a = Builder
    { runBuilder :: Integer -> (Integer, a)
      -- ^ Runs the builder.
    }

-- | Evaluates the builder.
evalBuilder :: Builder a -> Integer -> a
evalBuilder b n = snd $ runBuilder b n

-- | Executes the builder.
execBuilder :: Builder a -> Integer -> Integer
execBuilder b n = fst $ runBuilder b n

instance Functor Builder where
    fmap f (Builder g) = Builder $ second f . g

instance Applicative Builder where
    pure x = Builder (\ n -> (n, x))
    f <*> x = Builder $ \ n ->
        let (n', g) = runBuilder f n
        in fmap g $ runBuilder x n'

instance Monad Builder where
    return = pure
    x >>= f = Builder $ \ n ->
        let (n', v) = runBuilder x n
        in runBuilder (f v) n'

instance Monoid a => Monoid (Builder a) where
    mempty = return mempty
    mappend a b = mappend <$> a <*> b

-- | Returns a positive integer less than `i`.
lessThan :: Integral a => a -> Builder a
lessThan i = Builder $ \ n -> second fromIntegral $ quotRem n $ fromIntegral i

-- | Returns an integer between `a` and `b`, both inclusive.
inRange :: Integral a => (a, a) -> Builder a
inRange (a, b) = fmap (+ a) $ lessThan $ b + 1 - a

-- | Returns a lower case letter.
lower :: Builder Char
lower = fmap (chr . (+ ord 'a')) $ lessThan 26

-- | Returns an upper case letter.
upper :: Builder Char
upper = fmap (chr . (+ ord 'A')) $ lessThan 26

-- | Returns an printable ascii char.
ascii :: Builder Char
ascii = fmap chr $ inRange (32, 126)

-- | Returns a digit.
digit :: Builder Char
digit = fmap chr $ inRange (48, 57)

-- | Returns a letter.
letter :: Builder Char
letter = join $ oneOf [upper, lower]

-- | Returns a special character.
special :: Builder Char
special = oneOf "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

-- | Returns one element of the list.
oneOf :: [a] -> Builder a
oneOf [] = error "oneOf on empty list"
oneOf xs = fmap (xs !!) $ lessThan $ length xs

-- | Returns on element of the vector.
oneOfV :: Vector a -> Builder a
oneOfV vect = fmap (vect V.!) $ lessThan $ V.length vect

{- | Returns the results of the input builder
     until the consummed integer is 0. -}
useup :: Builder a -> Builder [a]
useup b = Builder $ \ n ->
    if n == 0 then (0, []) else runBuilder
        ((:) <$> b <*> useup b) n

-- | Shuffles the input list.
shuffle :: [a] -> Builder [a]
shuffle xs = fmap (perm xs) $ lessThan $ fact $ length xs
  where
    fact :: Int -> Int
    fact n = product [1 .. n]
