module Prop.Unique
( uniqueProps
) where

import Data.List
import Data.Word
import Test.QuickCheck

import GoatSwim.TimeFrame
import GoatSwim.Util
import GoatSwim.ValueFrame

-- | All properties that test for conversion uniqueness.
uniqueProps :: [(String, Property)]
uniqueProps =
  [ ("uniqueToBools",       property uniqueToBools)
  , ("uniqueFromBools",     property uniqueFromBools)
  , ("uniqueEncTimeFrame",  property uniqueEncTimeFrame)
  , ("uniqueEncValueFrame", property uniqueEncValueFrame) ]

-- | Two different words should always map to two different bit lists and two
-- equal words should always map to equal lists.
uniqueToBools :: Word16
              -> Word16
              -> Bool
uniqueToBools x y
  | x == y    = toBools x == toBools y
  | otherwise = toBools x /= toBools y

-- | Two different words should always map to two different bit lists and two
-- equal words should always map to equal lists.
uniqueFromBools :: [Bool]
                -> [Bool]
                -> Bool
uniqueFromBools xs ys
  | as == bs  = a == b
  | otherwise = a /= b
  where
    a = fromBools as :: Word16
    b = fromBools bs :: Word16
    as = take len xs
    bs = take len ys
    len = minimum [16, length xs, length ys]

-- | Two different time series always map to two different TimeFrames and
-- equal time series always map to equal TimeFrames.
uniqueEncTimeFrame :: [Word32]
                   -> [Word32]
                   -> Bool
uniqueEncTimeFrame xs ys
  | as == bs  = timeEncode as == timeEncode bs
  | otherwise = timeEncode as /= timeEncode bs
  where
    as   = norm xs
    bs   = norm ys
    norm = (nub . sort)

-- | Two diffent data series always map to two diffent ValueFrames and
-- equal data series always map to equal ValueFrames.
uniqueEncValueFrame :: [Float]
                    -> [Float]
                    -> Bool
uniqueEncValueFrame xs ys
  | xs == ys  = valueEncode xs == valueEncode ys
  | otherwise = valueEncode xs /= valueEncode ys

