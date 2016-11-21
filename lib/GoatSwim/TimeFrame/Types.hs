module GoatSwim.TimeFrame.Types
( TimeFrame(..)
) where

import Data.Word

import qualified Data.ByteString as B

import GoatSwim.Util

-- | Succinct representation of a set of time points.
data TimeFrame = TimeFrame
                 (Maybe Word32) -- ^ first time point
                 (Maybe Word32) -- ^ second time point
                 Int            -- ^ number of valid bits
                 B.ByteString   -- ^ bits
                 deriving (Eq)

-- | Pretty-printing of the TimeFrame type.
instance Show TimeFrame where
  show (TimeFrame Nothing  _        _   _ ) = "TimeFrame EMPTY"
  show (TimeFrame (Just x) Nothing  _   _ ) = "TimeFrame " ++ show x 
  show (TimeFrame (Just x) (Just y) len bs) =
    unwords [ "TimeFrame"
            , "frst=" ++ show x
            , "scnd=" ++ show y 
            , map (bool '1' '0') (take len (unpackBits bs)) ]
