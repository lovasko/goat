{- |
Module      : Codec.Goat.TimeFrame.Encode
Description : Time compression
Copyright   : (c) Daniel Lovasko, 2016-2017
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Encoding of time points into a compressed frame form.
-}

module Codec.Goat.TimeFrame.Encode
( timeEncode
) where

import Data.Int
import Data.List
import Data.Word
import qualified Data.ByteString as B

import Codec.Goat.TimeFrame.Types
import Codec.Goat.Util


-- | Pack a list of _ascending_ time points into a succinct frame form.
timeEncode :: [Word32]  -- ^ time points
           -> TimeFrame -- ^ succinct frame form
timeEncode []       = TimeFrame Nothing  Nothing  0     B.empty
timeEncode [x]      = TimeFrame (Just x) Nothing  0     B.empty
timeEncode [x, y]   = TimeFrame (Just x) (Just y) 0     B.empty
timeEncode (x:y:zs) = TimeFrame (Just x) (Just y) valid (packBits bits)
  where
    valid  = genericLength bits
    bits   = concatMap encode dods                 :: [Bool]
    dods   = zipWith (-) deltas (sub y x : deltas) :: [Int64]
    deltas = zipWith sub zs (y:zs)                 :: [Int64]

-- | Encode the new incoming value based on its delta of a delta.
encode :: Int64  -- ^ delta of a delta
       -> [Bool] -- ^ new bits
encode dod
  | dod == 0                   = header 0 1
  | inBounds (  -64,   63) dod = header 1 1 ++ encodeNumber  7 dod
  | inBounds ( -256,  255) dod = header 2 1 ++ encodeNumber  9 dod
  | inBounds (-2048, 2047) dod = header 3 1 ++ encodeNumber 12 dod
  | otherwise                  = header 4 0 ++ toBools dod
  where
    header t f = replicate t True ++ replicate f False
    encodeNumber valid x = take valid $ toBools (x + (2 ^ (valid - 1)))
