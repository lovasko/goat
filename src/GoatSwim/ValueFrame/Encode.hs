module GoatSwim.ValueFrame.Encode
( valueEncode
) where

import Data.Bits
import Data.Bits.Floating
import Data.List
import Data.Word

import qualified Data.ByteString as B

import GoatSwim.Util
import GoatSwim.ValueFrame.Types

-- | Encode a list of float values into a succinct value frame.
valueEncode :: [Float]    -- ^ data values
            -> ValueFrame -- ^ succinct frame form
valueEncode [] = ValueFrame Nothing 0 B.empty
valueEncode xs = ValueFrame (Just x) (length bits) (packBits bits)
  where
    bits      = concat $ snd $ mapAccumL encode (12, 12) xors :: [Bool]
    xors      = zipWith xor words (tail words)                :: [Word32]
    (x:words) = map coerceToWord xs                           :: [Word32]

-- | Encode a single value based on the previous leading and trailing
-- bit count.
--encode :: (Int, Int)           -- ^ current leading/trailing zeros
--       -> Word32               -- ^ value
--       -> ((Int, Int), [Bool]) -- ^ bits and new leading/trailing
--encode keep@(lead, trail) x
--  | x == 0    = (keep, [False])
--  | fits      = (keep, [True, False] ++ slice lead trail x)
--  | otherwise = (new,  [True, True]  ++ start 5 newLead
--                                     ++ start 6 (32-newLead-newTrail)
--                                     ++ slice newLead newTrail x)
--    where
--      new@(newL, newT) = (countLeadingZeros &&& countTrailingZeros) x
--      fits             = lead <= newL || trail <= newT
--      slice l t n      = take (32-t-l) (drop l (toListLE n))
--      start t n        = take t (toListLE n)

-- | Encode a single value based on the previous leading and trailing
-- bit count.
encode :: (Int, Int)           -- ^ current leading/trailing zeros
       -> Word32               -- ^ value
       -> ((Int, Int), [Bool]) -- ^ bits and new leading/trailing
encode bounds x
  | x == 0    = (bounds, [False])
  | fits      = (bounds, [True, False] ++ slice bounds bits)
  | otherwise = (core,   [True, True]  ++ outside core bits)
  where
    fits = within bounds core :: Bool
    core = meaningful bits    :: (Int, Int)
    bits = toBools x          :: [Bool]

-- | Handle the encoding case where the core part of the word does not fit
-- into the rolling bounds.
-- TODO why is it 5 and 6
outside :: (Int, Int) -- ^ bounds
        -> [Bool]     -- ^ all number bits
        -> [Bool]     -- ^ encoded bits
outside bounds@(lead, trail) bits = (take 5 $ toBools lead)            ++
                                    (take 6 $ toBools (32-lead-trail)) ++
                                    slice bounds bits

-- | Select a sublist based on the specified bounds.
slice :: (Int, Int) -- ^ bounds
      -> [a]        -- ^ list
      -> [a]        -- ^ sublist
slice (lead, trail) xs = take (32-lead-trail) (drop lead xs)

-- | Check whether bounds like within other bounds.
within :: (Int, Int) -- ^ existing bounds
       -> (Int, Int) -- ^ new bounds
       -> Bool       -- ^ decision
within (x, y) (a, b) = x <= a && y >= b

-- | Find the core of the word surrounded by zero bits from both sides.
meaningful :: [Bool]     -- ^ bits 
           -> (Int, Int) -- ^ non-zero core bounds
meaningful bits = (lead, trail)
  where
    lead  = length $ takeWhile (== False) bits
    trail = length $ takeWhile (== False) (reverse bits)

