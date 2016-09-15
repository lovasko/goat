{-# LANGUAGE MultiParamTypeClasses #-}

module GoatSwim.Frame
( Frame
, frameDecode
, frameEncode
, frameHead
) where

-- | Abstract compression frame that binds two types together. The type
-- variable "r" stands for Raw, variable "c" for Compressed.
class Frame r c where
  frameDecode :: c -> [r]     -- ^ decompress raw values
  frameEncode :: [r] -> c     -- ^ compress raw values
  frameHead   :: c -> Maybe r -- ^ first compressed raw value

