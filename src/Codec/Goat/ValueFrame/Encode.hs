{- |
Module      : Codec.Goat.ValueFrame.Encode
Description : Value compression
Copyright   : (c) Daniel Lovasko, 2016-2017
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Encoding of data point values into a compressed frame form.
-}

module Codec.Goat.ValueFrame.Encode
( valueEncode
) where

import Control.Arrow
import Data.Bits
import Data.Bits.Floating
import Data.List
import Data.Word
import qualified Data.ByteString as B

import Codec.Goat.Util
import Codec.Goat.ValueFrame.Types

-- | Encode a list of float values into a succinct value frame.
valueEncode :: [Float]    -- ^ data values
            -> ValueFrame -- ^ succinct frame form
valueEncode [] = ValueFrame Nothing  0                    B.empty
valueEncode xs = ValueFrame (Just y) (genericLength bits) (packBits bits)
  where
    bits   = concat $ snd $ mapAccumL encode (16, 16) xors :: [Bool]
    xors   = zipWith xor (y:ys) ys                         :: [Word32]
    (y:ys) = map coerceToWord xs                           :: [Word32]

-- | Encode a single value based on the previous bounds.
encode :: (Int, Int)           -- ^ current bounds
       -> Word32               -- ^ value
       -> ((Int, Int), [Bool]) -- ^ new bounds & encoded bits
encode bounds x
  | x == 0    = (bounds,    [False])
  | fits      = (bounds,    [True, False] ++ slice bounds bits)
  | otherwise = (newBounds, [True, True]  ++ outside newBounds bits)
  where
    fits      = within bounds newBounds
    newBounds = (countTrailingZeros &&& countLeadingZeros) x
    bits      = toBools x

-- | Handle the encoding case where the core part of the word does not fit
-- into the rolling bounds.
outside :: (Int, Int) -- ^ bounds
        -> [Bool]     -- ^ all bits of a number
        -> [Bool]     -- ^ encoded bits
outside bounds@(lead, trail) bits = concat
  [ take 5 $ toBools lead
  , take 6 $ toBools $ 32-lead-trail
  , slice bounds bits ]

-- | Select a sublist based on the specified bounds. Note that this
-- functions assumes list length to be 32.
slice :: (Int, Int) -- ^ bounds
      -> [a]        -- ^ list
      -> [a]        -- ^ sublist
slice (lead, trail) xs = take (32-lead-trail) (drop lead xs)

-- | Check whether bounds like within other bounds.
within :: (Int, Int) -- ^ existing bounds
       -> (Int, Int) -- ^ new bounds
       -> Bool       -- ^ decision
within (a, b) (na, nb) = na >= a && nb >= b
