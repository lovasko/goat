module Prop.Fluid
( fluidProps
) where

import Test.QuickCheck

import Codec.Goat.Fluid
import Codec.Goat.ValueFrame

-- | All tests assert the correct functionality of the Fluid type.
fluidProps :: [(String, Property)]
fluidProps = [ ("fluidAppendShiftDump", property fluidAppendShiftDump) ]

-- | Append each inner list to the fluid element by element and issue
-- a shifrt after every list.
appendShift :: [[Float]]              -- ^ values
            -> Fluid Float ValueFrame -- ^ fluid
appendShift xs = fill (reverse xs) new
  where
    new            = fluidNew (4, maxBound)
    fill []     fl = fl
    fill (y:ys) fl = fill ys (fluidShift $ foldr (flip fluidAppend) fl y)

-- | Given a random set of values and shifts, the fluid object must
-- retain all data stored in it. Moreover, this data has to be in the
-- correct order.
fluidAppendShiftDump :: [[Float]] -- ^ values
                     -> Bool      -- ^ decision
fluidAppendShiftDump xs = concat xs == fluidDump (appendShift xs)

