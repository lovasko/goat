{- |
Module      : Codec.Goat.Frame
Description : Abstraction over different compressions
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

The Frame typeclass unites the interface to both TimeFrame and ValueFrame
types. It is inteded only for internal use.
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
  frameDecode :: c -> [r]     -- ^ decompress raw values
  frameEncode :: [r] -> c     -- ^ compress raw values
  frameHead   :: c -> Maybe r -- ^ first compressed raw value

instance Frame Float ValueFrame where
  frameDecode = valueDecode
  frameEncode = valueEncode
  frameHead (ValueFrame first _ _) = fmap coerceToFloat first

instance Frame Word32 TimeFrame where
  frameDecode = timeDecode
  frameEncode = timeEncode
  frameHead (TimeFrame first _ _ _) = first