{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module GoatSwim.TimeFrame.Types
( TimeFrame(..)
) where

import Control.DeepSeq
import GHC.Generics
import GoatSwim.Util
import Data.Word
import qualified Data.ByteString as B

-- | Succinct representation of a set of time points.
data TimeFrame = TimeFrame
                 (Maybe Word32) -- ^ first time point
                 (Maybe Word32) -- ^ second time point
                 Int            -- ^ number of valid bits
                 B.ByteString   -- ^ bits
                 deriving (Generic, NFData)

-- | Pretty-printing of the TimeFrame type.
instance Show TimeFrame where
  show (TimeFrame Nothing     _           _   _ ) = "TimeFrame EMPTY"
  show (TimeFrame (Just frst) Nothing     _   _ ) = "TimeFrame " ++ show frst
  show (TimeFrame (Just frst) (Just scnd) len bs) =
    unwords [ "TimeFrame"
            , "frst=" ++ show frst
            , "scnd=" ++ show scnd
            , map (bool '1' '0') (take len (unpackBits bs)) ]

