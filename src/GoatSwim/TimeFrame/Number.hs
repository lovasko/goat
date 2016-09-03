module GoatSwim.TimeFrame.Number
( encodeNumber
, decodeNumber
) where

import Data.Bits.Bitwise (fromListLE, toListLE)
import Data.Int

-- | Convert an integer with bias into bits representation.
encodeNumber :: Int    -- ^ valid bits
             -> Int64  -- ^ number
             -> [Bool] -- ^ bits
encodeNumber valid x = take valid $ toListLE (x + (2 ^ (valid - 1)))

-- | Convert a list of bits with bias into an integer.
decodeNumber :: Int    -- ^ valid bits
             -> [Bool] -- ^ bits
             -> Int64  -- ^ number
decodeNumber valid bits = fromListLE bits - (2 ^ (valid - 1))

