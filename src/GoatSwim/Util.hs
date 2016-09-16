module GoatSwim.Util
( bool
, alignTo
, fromBools
, inBounds
, packBits
, sub
, toBools
, unpackBits
) where

import Data.Bits
import Data.Int
import Data.List.Split (chunksOf)
import Data.Word

import qualified Data.ByteString as B

-- | Check whether a value falls within bounds (borders included).
inBounds :: (Ord a)
         => a    -- ^ lower bound
         -> a    -- ^ upper bound
         -> a    -- ^ value
         -> Bool -- ^ decision
inBounds lower upper value = lower <= value && value <= upper

-- | Semantically correct substitution of two words.
sub :: Word32 -- ^ first word
    -> Word32 -- ^ second word
    -> Int64  -- ^ result
sub a b = fromIntegral a - fromIntegral b

-- | Pack a list of bits into a more compact form.
packBits :: [Bool]       -- ^ bits
         -> B.ByteString -- ^ bytestring
packBits xs = B.pack $ map fromBools (chunksOf 8 xs)

-- | Unpack a compact block of bytes into a list of bools.
unpackBits :: B.ByteString -- ^ bits
           -> [Bool]       -- ^ bytestring
unpackBits b = concatMap toBools (B.unpack b)

-- | Endomorphism on the boolean type.
bool :: a    -- ^ True option
     -> a    -- ^ False option
     -> Bool -- ^ bool
     -> a    -- ^ result
bool x _ True  = x
bool _ y False = y

-- | Align a list with a default element so that the list's length is a
-- multiple of a specified number.
alignTo :: Int -- ^ base size
        -> a   -- ^ element to append
        -> [a] -- ^ old list
        -> [a] -- ^ new list
alignTo n e xs
  | rear == 0 = xs
  | otherwise = xs ++ replicate (n - rear) e
  where
    rear = mod (length xs) n

-- | Convert a Bits instance into a list of bools.
toBools :: (FiniteBits b)
        => b      -- ^ Bits instance
        -> [Bool] -- ^ bits
toBools bits = map (testBit bits) [0..finiteBitSize bits-1]

-- | Convert a list of bools into a Bits instance.
fromBools :: (Num b, FiniteBits b)
          => [Bool] -- ^ bits
          -> b      -- ^ Bits instance
fromBools = foldr (\b i -> bool (bit 0) 0 b .|. shift i 1) 0

