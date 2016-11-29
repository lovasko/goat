module GoatSwim.ValueFrame.Types
( ValueFrame(..)
) where

import Data.Bits.Floating
import Data.List
import Data.Serialize
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

-- | Binary serialization of the ValueFrame type.
instance Serialize ValueFrame where
  -- | Encoding of the ValueFrame type. All integers are encoded with
  -- big-endian byte order.
  put (ValueFrame x len bs) = do
    putMaybeOf putWord32be x
    putWord32be len
    aiPutByteString bs
--    return ()

  -- | Decoding of the ValueFrame type.
  get = do
    x   <- getMaybeOf getWord32be
    len <- getWord32be
    bs  <- aiGetByteString
    return $ ValueFrame x len bs