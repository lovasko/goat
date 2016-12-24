{- |
Module      : Codec.Goat.ValueFrame
Description : Top-level header for value compression
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable
-}

module GoatSwim.ValueFrame
( ValueFrame(..)
, valueDecode
, valueEncode
) where

import GoatSwim.ValueFrame.Decode
import GoatSwim.ValueFrame.Encode
import GoatSwim.ValueFrame.Types
