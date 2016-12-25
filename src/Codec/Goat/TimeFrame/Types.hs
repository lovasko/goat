{- |
Module      : Codec.Goat.TimeFrame.Types
Description : TimeFrame type definition
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

The TimeFrame type represents the compressed form of data point timestamps.
It is an instance of Eq and Serialize typeclasses.
-}

module Codec.Goat.TimeFrame.Types
( TimeFrame(..)
) where

import Data.List
import Data.Serialize
import Data.Word
import qualified Data.ByteString as B

import Codec.Goat.Util

-- | Succinct representation of a set of time points.
data TimeFrame = TimeFrame
                 (Maybe Word32) -- ^ first time point
                 (Maybe Word32) -- ^ second time point
                 Word32         -- ^ number of valid bits
                 B.ByteString   -- ^ bits
                 deriving (Eq)

-- | Pretty-printing of the TimeFrame type.
instance Show TimeFrame where
  show (TimeFrame Nothing  _        _   _ ) = "TimeFrame EMPTY"
  show (TimeFrame (Just x) Nothing  _   _ ) = "TimeFrame " ++ show x
  show (TimeFrame (Just x) (Just y) len bs) = unwords
    [ "TimeFrame"
    , "frst=" ++ show x
    , "scnd=" ++ show y
    , map (bool '1' '0') (genericTake len (unpackBits bs)) ]

-- | Binary serialization of the TimeFrame type. All integers are using
-- the big-endian byte order.
instance Serialize TimeFrame where
  put (TimeFrame x y len bs) = do
    putMaybeOf putWord32be x
    putMaybeOf putWord32be y
    putWord32be len
    aiPutByteString bs
  get = do
    x   <- getMaybeOf getWord32be
    y   <- getMaybeOf getWord32be
    len <- getWord32be
    bs  <- aiGetByteString
    return $ TimeFrame x y len bs
