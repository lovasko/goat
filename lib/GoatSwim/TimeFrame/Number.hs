module GoatSwim.TimeFrame.Number
( decodeNumber
, encodeNumber 
) where

import Data.Int

import GoatSwim.Util

-- | Convert an integer with bias into bits representation.
encodeNumber :: Int    -- ^ valid bits
             -> Int64  -- ^ number
             -> [Bool] -- ^ bits
encodeNumber valid x = take valid $ toBools (x + (2 ^ (valid - 1)))

-- | Convert a list of bits with bias into an integer.
decodeNumber :: Int    -- ^ valid bits
             -> [Bool] -- ^ bits
             -> Int64  -- ^ number
decodeNumber valid bits = fromBools bits - (2 ^ (valid - 1))
