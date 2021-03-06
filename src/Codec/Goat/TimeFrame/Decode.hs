{- |
Module      : Codec.Goat.TimeFrame.Decode
Description : Time decompression
Copyright   : (c) Daniel Lovasko, 2016-2017
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Decoding of the compressed frame form into raw time points.
-}

module Codec.Goat.TimeFrame.Decode
( timeDecode
) where

import Data.Int
import Data.List
import Data.Word
import qualified Data.ByteString as B

import Codec.Goat.TimeFrame.Types
import Codec.Goat.Util


-- | Unpack time points from the succinct frame.
timeDecode :: TimeFrame -- ^ succinct frame form
           -> [Word32]  -- ^ time points
timeDecode (TimeFrame Nothing  _        _   _ ) = []
timeDecode (TimeFrame (Just x) Nothing  _   _ ) = [x]
timeDecode (TimeFrame (Just x) (Just y) len bs)
  | B.null bs || len == 0 = [x, y]
  | otherwise             = [x, y] ++ map fromIntegral times
  where
    times  = apply (fromIntegral y) deltas   :: [Int64]
    deltas = apply (sub y x) dods            :: [Int64]
    dods   = unfoldr decode bits             :: [Int64]
    bits   = genericTake len (unpackBits bs) :: [Bool]
    apply n xs = drop 1 $ scanl (+) n xs

-- | Decode a single delta of a delta from a list of bits.
decode :: [Bool]                -- ^ bits
       -> Maybe (Int64, [Bool]) -- ^ delta of a delta & bits
decode (False:xs)                = Just (0, xs)
decode (True:False:xs)           = Just $ extract xs  7
decode (True:True:False:xs)      = Just $ extract xs  9
decode (True:True:True:False:xs) = Just $ extract xs 12
decode (True:True:True:True:xs)  = Just $ first fromBools (splitAt 64 xs)
decode _                         = Nothing

-- | Extract a signed integer from a list of bits.
extract :: [Bool]          -- ^ bits
        -> Int             -- ^ valid bits
        -> (Int64, [Bool]) -- ^ number & bits
extract xs n = first (decodeNumber n) (splitAt n xs)
  where decodeNumber valid bits = fromBools bits - (2 ^ (valid - 1))
