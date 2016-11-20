{-# LANGUAGE MultiParamTypeClasses #-}

module GoatSwim.Frame
( Frame
, frameDecode
, frameEncode
, frameHead
) where

import Data.Bits.Floating
import Data.Word

import GoatSwim.TimeFrame
import GoatSwim.ValueFrame

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

