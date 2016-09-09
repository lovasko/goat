module GoatSwim.ValueFrame.Types
( ValueFrame(..)
) where

import Data.Bits.Floating
import Data.Word
import GoatSwim.Util
import qualified Data.ByteString as B

-- | Succinct representation of a set of values.
data ValueFrame = ValueFrame
                  (Maybe Word32) -- ^ first value
                  Int            -- ^ number of valid bits
                  B.ByteString   -- ^ bits

-- | Pretty-printing of the ValueFrame type.
instance Show ValueFrame where
  show (ValueFrame Nothing _ _)       = "ValueFrame EMPTY"
  show (ValueFrame (Just x) len bits) =
    unwords [ "ValueFrame"
            , "first=" ++ show (coerceToFloat x :: Float)
            , take len $ map (bool '1' '0') (unpackBits bits) ]

