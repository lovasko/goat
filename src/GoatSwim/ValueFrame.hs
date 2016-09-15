{-# LANGUAGE MultiParamTypeClasses #-}

module GoatSwim.ValueFrame
( ValueFrame(..)
, valueDecode
, valueEncode
) where

import Data.Bits.Floating

import GoatSwim.Frame
import GoatSwim.ValueFrame.Decode
import GoatSwim.ValueFrame.Encode
import GoatSwim.ValueFrame.Types

instance Frame Float ValueFrame where
  frameDecode = valueDecode
  frameEncode = valueEncode
  frameHead (ValueFrame first _ _) = fmap coerceToFloat first

