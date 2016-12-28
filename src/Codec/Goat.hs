{- |
Module      : Codec.Goat
Description : Top-level module file
Copyright   : (c) Daniel Lovasko, 2016
License     : BSD3

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable

Goat is a time series compression implementation in pure Haskell. It is
heavily based on Facebook's Gorilla algorithm [1].

[1] http://www.vldb.org/pvldb/vol8/p1816-teller.pdf
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
