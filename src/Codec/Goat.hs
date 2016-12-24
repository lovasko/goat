{- |
Module      : Codec.Goat
Description : Top-level module file
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Goat is a time series compression algorithm implementation in pure Haskell.
-}

module Codec.Goat
( Story(..)
, TimeFrame(..)
, ValueFrame(..)
, storyAppend
, storyDump
, storyNew
, storyQuery
, timeDecode
, timeEncode
, valueDecode
, valueEncode
) where

import Codec.Goat.Story
import Codec.Goat.TimeFrame
import Codec.Goat.ValueFrame
