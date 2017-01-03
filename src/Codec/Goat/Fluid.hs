{- |
Module      : Codec.Goat.Fluid
Description : Transition between compressed and raw data
Copyright   : (c) Daniel Lovasko, 2016-2017
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

By using the Frame abstraction, the Fluid type achieves transition
between the raw and compressed states for both TimeFrame and ValueFrame.
It is intended for internal use only.
-}

module Codec.Goat.Fluid
( Fluid(..)
, fluidAppend
, fluidDump
, fluidFirst
, fluidHeads
, fluidNew
, fluidSelect
, fluidShift
) where

import Safe

import Codec.Goat.Frame
import Codec.Goat.Util

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
fluidFirst (Fluid _ []    _) = Nothing
fluidFirst (Fluid _ (x:_) _) = headMay x

-- | Obtain the first elements of each section blocks. Empty frames and
-- lists are represented as Nothing.
fluidHeads :: Frame r c
           => Fluid r c -- ^ fluid
           -> [Maybe r] -- ^ block heads
fluidHeads (Fluid _ rs cs) = map headMay rs ++ map frameHead cs

-- | Return raw values stored within specified block indices.
fluidSelect :: Frame r c
            => Fluid r c -- ^ fluid
            -> [Bool]    -- ^ block presence
            -> [r]       -- ^ raw values
fluidSelect (Fluid _ rs cs) presence = rvalues ++ cvalues
  where
    (rpres, cpres) = splitAt (length rs) presence
    rvalues        = concat $ select rs rpres
    cvalues        = concatMap frameDecode (select cs cpres)

-- | Shift one uncompressed component into the compressed ones and create
-- a new empty uncompressed component.
fluidShift :: Frame r c
           => Fluid r c -- ^ old fluid
           -> Fluid r c -- ^ new fluid
fluidShift (Fluid ls@(l1, l2) rs cs)
  | (null . last) rs || (length rs < l1) = Fluid ls newRs cs
  | otherwise                            = Fluid ls newRs newCs
  where
    newRs = []                    : bool rs (init rs) (length rs < l1)
    newCs = frameEncode (last rs) : bool cs (init cs) (length cs < l2)

-- | Add new value to the fluid. Internally this function is _prepending_
-- in front of all existing values.
fluidAppend :: Fluid r c -- ^ old fluid
            -> r         -- ^ new value
            -> Fluid r c -- ^ new fluid
fluidAppend (Fluid ls []     cs) val = Fluid ls [[val]]      cs
fluidAppend (Fluid ls (r:rs) cs) val = Fluid ls ((val:r):rs) cs

-- | Dump all stored values in the uncompressed form.
fluidDump :: Frame r c
          => Fluid r c  -- ^ fluid
          -> [r]        -- ^ uncompressed values
fluidDump (Fluid _ rs cs) = concat rs ++ concatMap frameDecode cs
