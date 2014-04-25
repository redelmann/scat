
-- | Password scatterer.
module Scat (scatter) where

import Data.Monoid
import Data.ByteString (ByteString, unpack)
import qualified Data.ByteString.Char8 as C
import Crypto.Scrypt

-- | Generates the seed integer given a service, a password and a code.
scatter :: ByteString -> ByteString -> ByteString -> Integer
scatter k pw c = foldr (\ n s -> fromIntegral n + 256 * s) 0 $
    unpack $ getHash $ scrypt params (Salt k) (Pass $ pw <> c)
  where
    Just params = scryptParams 14 8 50
