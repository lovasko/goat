{- |
Module      : Codec.Goat.Story
Description : High-level time series API
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

The Story type provides an easy-to-use high-level interface to a time
series object. This module implements the building of the time series
structure and enables interval queries that return data point values.
-}

module Codec.Goat.Story
( Story(..)
, storyAppend
, storyDump
, storyNew
, storyQuery
) where

import Data.Word

import Codec.Goat.Fluid
import Codec.Goat.TimeFrame
import Codec.Goat.Util
import Codec.Goat.ValueFrame

-- | Representation of a time series object.
data Story = Story
             Word32                   -- ^ time window size
             Word32                   -- ^ current time window number
             (Fluid Word32 TimeFrame) -- ^ times fluid
             (Fluid Float ValueFrame) -- ^ values fluid

-- | Pretty-printing for the Story type.
instance Show Story where
  show (Story wsz win ft fv) = unwords
    [ "Story"
    , "wsz="    ++ show wsz
    , "win="    ++ show win
    , "times="  ++ show ft
    , "values=" ++ show fv ]

-- | Create a new empty story. The ratio of raw/compressed sections is
-- 12/74. These fields will be configurable in the future versions of the
-- module.
storyNew :: Word32 -- ^ time window size
         -> Word32 -- ^ current time window number
         -> Story  -- ^ new story
storyNew wsz win = Story wsz win (fluidNew (12, 74)) (fluidNew (12, 74))

-- | Add new time/value pair to the story. The function will return
-- Nothing in case that the value was invalid (NaN or Infinity) or when
-- the time was invalid with respect to previously added times (breaking
-- the series monotonicity).
storyAppend :: Story           -- ^ old story
            -> (Word32, Float) -- ^ time & value
            -> Maybe Story     -- ^ new story
storyAppend (Story wsz win ft fv) (newTime, newValue)
  | invalid   = Nothing
  | otherwise = Just $ Story wsz win2 newFt newFv
  where
    valueInv = isNaN newValue || isInfinite newValue
    timeInv  = maybe True (newTime>) (fluidFirst ft)
    invalid  = valueInv || timeInv
    newWin   = mod newTime wsz
    win2     = bool win newWin (win == newWin)
    newFt    = fluidAppend (bool ft (fluidShift ft) (newWin == win)) newTime
    newFv    = fluidAppend (bool fv (fluidShift fv) (newWin == win)) newValue

-- | Query the story for time/value pairs within the specified time limit.
storyQuery :: Story             -- ^ story
           -> (Word32, Word32)  -- ^ interval
           -> [(Word32, Float)] -- ^ times & values
storyQuery (Story _ _ ft fv) ival
  | all (==False) heads = []
  | otherwise           = zip times values
  where
    times  = fluidSelect ft heads
    values = fluidSelect fv heads
    heads  = map (maybe False (inBounds ival)) (fluidHeads ft)

-- | Output a list of all time/value pairs stored in the story.
storyDump :: Story             -- ^ story
          -> [(Word32, Float)] -- ^ times & values
storyDump (Story _ _ ft fv) = zip (fluidDump ft) (fluidDump fv)
