{-# LANGUAGE MultiParamTypeClasses #-}

module GoatSwim.TimeFrame
( TimeFrame(..)
, timeDecode
, timeEncode
) where

import Data.Word

import GoatSwim.Frame
import GoatSwim.TimeFrame.Decode
import GoatSwim.TimeFrame.Encode
import GoatSwim.TimeFrame.Types

instance Frame Word32 TimeFrame where
  frameDecode = timeDecode
  frameEncode = timeEncode
  frameHead (TimeFrame first _ _ _) = first

