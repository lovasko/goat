{- |
Module      : Codec.Goat.ValueFrame.Types
Description : ValueFrame type definition
Copyright   : (c) Daniel Lovasko, 2016-2017
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

The ValueFrame type represents the compressed form of data point values.
It is an instance of Eq and Serialize typeclasses.
-}

module Codec.Goat.ValueFrame.Types
( ValueFrame(..)
) where

import Data.Bits.Floating
import Data.List
import Data.Serialize
import Data.Word
import qualified Data.ByteString as B

import Codec.Goat.Util

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

-- | Binary serialization of the ValueFrame type. All integers are using
-- the big-endian byte order.
instance Serialize ValueFrame where
  put (ValueFrame x len bs) = do
    putMaybeOf putWord32be x
    putWord32be len
    aiPutByteString bs
  get = do
    x   <- getMaybeOf getWord32be
    len <- getWord32be
    bs  <- aiGetByteString
    return $ ValueFrame x len bs
