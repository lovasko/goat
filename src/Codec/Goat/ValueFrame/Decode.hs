{- |
Module      : Codec.Goat.ValueFrame.Decode
Description : Value decompression
Copyright   : (c) Daniel Lovasko, 2016-2017
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Decoding of the compressed frame form into raw value points.
-}

module Codec.Goat.ValueFrame.Decode
( valueDecode
) where

import Data.Bits
import Data.Bits.Floating
import Data.List
import Data.List.Split
import Data.Word
import qualified Data.ByteString as B

import Codec.Goat.ValueFrame.Types
import Codec.Goat.Util


-- | Helper type to hold the decoding context.
type Context = ([Bool], (Int, Int)) -- ^ available bits & bounds

-- | Unpack value points from the succinct frame.
valueDecode :: ValueFrame -- ^ succinct frame form
            -> [Float]    -- ^ value points
valueDecode (ValueFrame Nothing  _   _ ) = []
valueDecode (ValueFrame (Just x) len bs)
  | B.null bs || len == 0 = [coerceToFloat x]
  | otherwise             = map coerceToFloat (x:xors)
  where
    xors = drop 1 $ scanl xor x ws         :: [Word32]
    ws   = unfoldr decode (bits, (16, 16)) :: [Word32]
    bits = genericTake len (unpackBits bs) :: [Bool]

-- | Decode a single XORed value.
decode :: Context                 -- ^ current context
       -> Maybe (Word32, Context) -- ^ decoded value & new context
decode (False:xs,      bounds) = Just (0, (xs, bounds))
decode (True:False:xs, bounds) = Just $ inside bounds xs
decode (True:True:xs,  _     ) = Just $ outside xs
decode (_,             _     ) = Nothing

-- | Handling of the within-bounds decoding case.
inside :: (Int, Int)        -- ^ current bounds
       -> [Bool]            -- ^ bits
       -> (Word32, Context) -- ^ decoded number & context
inside bounds@(lead, trail) xs = (fromBools number, (ys, bounds))
  where
    number     = surround (lead, trail) bits
    (bits, ys) = splitAt (32 - lead - trail) xs

-- | Handling of the out-of-bounds decoding case.
outside :: [Bool]            -- ^ bits
        -> (Word32, Context) -- ^ decoded number & context
outside xs = (fromBools number, (ys, (lead, trail)))
  where
    [lead, size] = map fromBools $ splitPlaces ([5, 6] :: [Int]) xs
    trail        = 32 - lead - size
    number       = surround (lead, trail) bits
    (bits, ys)   = splitAt size (drop 11 xs)

-- | Surround a list of bools with False elements.
surround :: (Int, Int) -- ^ leading and trailing count
         -> [Bool]     -- ^ old list
         -> [Bool]     -- ^ new list
surround (lead, trail) = (replicate lead False ++) . (++ replicate trail False)
