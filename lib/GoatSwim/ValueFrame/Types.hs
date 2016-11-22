module GoatSwim.ValueFrame.Types
( ValueFrame(..)
) where

import Data.Bits.Floating
import Data.List
import Data.Word
import qualified Data.ByteString as B

import GoatSwim.Util

-- | Succinct representation of a set of values.
data ValueFrame = ValueFrame
                  (Maybe Word32) -- ^ first value
                  Word32         -- ^ number of valid bits
                  B.ByteString   -- ^ bits
                  deriving (Eq)

-- | Pretty-printing of the ValueFrame type.
instance Show ValueFrame where
  show (ValueFrame Nothing  _   _ ) = "ValueFrame EMPTY"
  show (ValueFrame (Just x) len bs) = unwords
    [ "ValueFrame"
    , "first=" ++ show (coerceToFloat x :: Float)
    , genericTake len $ map (bool '1' '0') (unpackBits bs) ]
