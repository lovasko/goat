{- |
Module      : Codec.Goat.TimeFrame
Description : Top-level header for time compression
Copyright   : (c) Daniel Lovasko, 2016-2017
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable
-}

module Codec.Goat.TimeFrame
( TimeFrame(..)
, timeDecode
, timeEncode
) where

import Codec.Goat.TimeFrame.Decode
import Codec.Goat.TimeFrame.Encode
import Codec.Goat.TimeFrame.Types
