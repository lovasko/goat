module GoatSwim.Fluid
( Fluid
, fluidAppend
, fluidDump
, fluidFirst
, fluidHeads
, fluidNew
, fluidSelect
, fluidShift
) where

import Safe

import GoatSwim.Frame
import GoatSwim.Util

-- | Management of fluid transitions between compressed and uncompresed
-- state.
data Fluid r c = Fluid
                 (Int, Int) -- ^ section lenghts
                 [[r]]      -- ^ uncompressed section
                 [c]        -- ^ compressed section

-- | Pretty-printing of the Fluid type.
instance Show (Fluid r c) where
  show (Fluid (l1, l2) rs cs) =
    unwords [ "Fluid"
            , "l1max=" ++ show l1
            , "l2max=" ++ show l2
            , "l1cur=" ++ show (length rs)
            , "l2cur=" ++ show (length cs) ]

-- | Create a new fluid.
fluidNew :: (Int, Int) -- ^ section lengths
         -> Fluid r c  -- ^ new fluid
fluidNew ls = Fluid ls [[]] []

-- | Obtain the first element stored in the frame.
fluidFirst :: Fluid r c
           -> Maybe r
fluidFirst (Fluid _ (x:_)   _) = headMay x

-- | Obtain the first elements of each section blocks. Empty frames and
-- lists are represented as Nothing.
fluidHeads :: (Frame r c)
           => Fluid r c -- ^ fluid
           -> [Maybe r] -- ^ block heads
fluidHeads (Fluid _ rs cs) = map headMay rs ++ map frameHead cs

-- | Return raw values stored within specified block indices.
fluidSelect :: Frame r c
            => Fluid r c  -- ^ fluid
            -> (Int, Int) -- ^ block indices
            -> [r]        -- ^ raw values
fluidSelect (Fluid _ rs cs) (a, b) = (concat $ drop a rs) ++ decRs
  where decRs = concatMap frameDecode (take (a+b-length rs) cs)

-- | Shift one uncompressed component into the compressed ones and create
-- a new empty uncompressed component.
fluidShift :: (Frame r c)
           => Fluid r c -- ^ old fluid
           -> Fluid r c -- ^ new fluid
fluidShift (Fluid ls@(l1, l2) rs cs) = Fluid ls newRs newCs
  where
    newRs = []                    : bool rs (init rs) (length rs <= l1)
    newCs = frameEncode (last rs) : bool cs (init cs) (length cs <= l2)

-- | Add new value to the fluid.
fluidAppend :: Fluid r c -- ^ old fluid
            -> r         -- ^ new value
            -> Fluid r c -- ^ new fluid
fluidAppend (Fluid ls (r:rs) cs) val = Fluid ls ((val:r):rs) cs

-- | Dump all stored values in the uncompressed form.
fluidDump :: (Frame r c)
          => Fluid r c  -- ^ fluid
          -> [r]        -- ^ uncompressed values
fluidDump (Fluid _ rs cs) = concat rs ++ concatMap frameDecode cs

