{- |
Module      : Codec.Goat.Frame
Description : Abstraction over different types of compression frames
Copyright   : (c) Daniel Lovasko, 2016-2017
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

The Frame typeclass unites the interface to both TimeFrame and ValueFrame
types. Intended for internal use only.
-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Goat.Frame
( Frame
, frameDecode
, frameEncode
, frameHead
) where

import Data.Bits.Floating
import Data.Word

import Codec.Goat.TimeFrame
import Codec.Goat.ValueFrame

-- | Abstract compression frame that binds two types together. The type
-- variable "r" stands for Raw, variable "c" for Compressed.
class Frame r c where
  frameDecode :: c -> [r]     -- ^ decompress a frame into raw elements
  frameEncode :: [r] -> c     -- ^ compress raw elements into a frame
  frameHead   :: c -> Maybe r -- ^ first raw element of a frame

-- | Binding of Float and ValueFrame.
instance Frame Float ValueFrame where
  frameDecode = valueDecode
  frameEncode = valueEncode
  frameHead (ValueFrame first _ _) = fmap coerceToFloat first

-- | Binding of Word32 and TimeFrame.
instance Frame Word32 TimeFrame where
  frameDecode = timeDecode
  frameEncode = timeEncode
  frameHead (TimeFrame first _ _ _) = first
