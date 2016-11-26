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
valueEncode [] = ValueFrame Nothing  0                    B.empty
valueEncode xs = ValueFrame (Just y) (genericLength bits) (packBits bits)
  where
    bits   = concat $ snd $ mapAccumL encode (16, 16) xors :: [Bool]
    xors   = zipWith xor (y:ys) ys                         :: [Word32]
    (y:ys) = map coerceToWord xs                           :: [Word32]

-- | Encode a single value based on the previous leading and trailing
-- bit count.
encode :: (Int, Int)           -- ^ current leading/trailing zeros
       -> Word32               -- ^ value
       -> ((Int, Int), [Bool]) -- ^ bits and new leading/trailing
encode bounds x
  | x == 0    = (bounds,    [False])
  | fits      = (bounds,    [True, False] ++ slice bounds bits)
  | otherwise = (newBounds, [True, True]  ++ outside newBounds bits)
  where
    fits      = within bounds newBounds
    newBounds = core x
    bits      = toBools x

-- | Handle the encoding case where the core part of the word does not fit
-- into the rolling bounds.
outside :: (Int, Int) -- ^ bounds
        -> [Bool]     -- ^ all number bits
        -> [Bool]     -- ^ encoded bits
outside bounds@(lead, trail) bits = take 5 ( toBools lead)            ++
                                    take 6 ( toBools (32-lead-trail)) ++
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
within (a, b) (na, nb) = na >= a && nb >= b

-- | Find the core of a 32-bit  word surrounded by zero bits from
-- both sides.
-- NOTE: this function swaps the lead/trail elements, as the functions
-- from the Data.Bits module interpret data differently.
core :: Word32     -- ^ 32-bit word
     -> (Int, Int) -- ^ non-zero core bounds
core x = (trail, lead)
  where
    lead  = countLeadingZeros x
    trail = countTrailingZeros x
