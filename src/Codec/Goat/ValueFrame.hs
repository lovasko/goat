{- |
Module      : Codec.Goat.ValueFrame
Description : Top-level header for value compression
Copyright   : (c) Daniel Lovasko, 2016-2017
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable
-}

module Codec.Goat.ValueFrame
( ValueFrame(..)
, valueDecode
, valueEncode
) where

import Codec.Goat.ValueFrame.Decode
import Codec.Goat.ValueFrame.Encode
import Codec.Goat.ValueFrame.Types
