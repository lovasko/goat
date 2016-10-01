module GoatSwim.Story
( Story(..)
, storyAppend
, storyNew
, storyQuery
, storyQueryAll
) where

import Data.Word

import GoatSwim.Fluid
import GoatSwim.TimeFrame
import GoatSwim.Util
import GoatSwim.ValueFrame

-- | Central type representing a time/value series.
data Story = Story
             Word32                   -- ^ time window size
             Word32                   -- ^ current time window
             (Fluid Word32 TimeFrame) -- ^ time points fluid
             (Fluid Float ValueFrame) -- ^ data points fluid

-- | Pretty-printinf for the Story type.
instance Show Story where
  show (Story wsz win ft fv) =
    unwords [ "Story"
            , "wsz="    ++ show wsz
            , "win="    ++ show win
            , "times="  ++ show ft
            , "values=" ++ show fv ]

-- | Create a new story.
storyNew :: Word32 -- ^ time window size
         -> Word32 -- ^ current time window
         -> Story  -- ^ new story
storyNew wsz win = Story wsz win (fluidNew (12, 74)) (fluidNew (12, 74))

-- | Add new time-data pair to the story. The function will return Nothing
-- in case that the data point was invalid or when the time point was
-- invalid with respect to already existing time points.
storyAppend :: Story           -- ^ old story
            -> (Word32, Float) -- ^ time-data pair
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

-- | Query the story for time-data pairs within specified time limits.
storyQuery :: Story             -- ^ story
           -> (Word32, Word32)  -- ^ queried interval
           -> [(Word32, Float)] -- ^ time/data pairs
storyQuery (Story _ _ ft fv) (hi, lo)
  | all (==False) heads = []
  | otherwise           = zip times values
  where
    times  = fluidSelect ft heads
    values = fluidSelect fv heads
    heads  = map (maybe False (inBounds hi lo)) (fluidHeads ft)

-- | Output a list of all time/value pairs stored for the metric.
storyQueryAll :: Story             -- ^ story
              -> [(Word32, Float)] -- ^ time/data pairs
storyQueryAll (Story _ _ ft fv) = zip (fluidDump ft) (fluidDump fv)

