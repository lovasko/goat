{- |
Module      : Codec.Goat.Util
Description : Various utility functions
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Various utility functions used throughout the project.
-}

module GoatSwim.Util
( aiGetByteString
, aiPutByteString
, bool
, first
, fromBools
, inBounds
, packBits
, select
, sub
, toBools
, unpackBits
) where

import Control.Applicative
import Data.Bits
import Data.Int
import Data.List.Split (chunksOf)
import Data.Serialize
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

-- | Select only certain elements from the list based on the boolean values.
select :: [a]    -- ^ list
       -> [Bool] -- ^ presence flags
       -> [a]    -- ^ filtered list
select []     _      = []
select _      []     = []
select (x:xs) (b:bs) = bool (x : select xs bs) (select xs bs) b

-- | Apply a function to the first element of a pair.
first :: (a -> b) -- ^ function
      -> (a, x)   -- ^ old pair
      -> (b, x)   -- ^ new pair
first f (a, x) = (f a, x)

-- | Architecture-independent serialization of a strict ByteString.
aiPutByteString :: B.ByteString -- ^ bytestring to parse
                -> Put          -- ^ writer
aiPutByteString bs = putListOf putWord8 (B.unpack bs)

-- | Architecture-independent deserialization of a lazy ByteString.
aiGetByteString :: Get B.ByteString -- ^ reader
aiGetByteString = B.pack <$> getListOf getWord8
