{-# OPTIONS_GHC -fno-warn-orphans #-}

module Prop.Id
( idProps
) where

import Data.List
import Data.Word
import Test.QuickCheck

import qualified Data.ByteString as B

import GoatSwim.TimeFrame
import GoatSwim.Util
import GoatSwim.ValueFrame


instance Arbitrary B.ByteString where
  arbitrary = fmap B.pack arbitrary
  shrink xs = fmap B.pack (shrink (B.unpack xs))

-- | All tests that assert the fact that encode/decode functions
-- must form an identity when composed.
idProps :: [(String, Property)]
idProps =
  [ ("idToFromBools",      property idToFromBools)
  , ("idFromToBools",      property idFromToBools)
  , ("idPackUnpackBits",   property idPackUnpackBits)
  , ("idUnpackPackBits",   property idUnpackPackBits)
  , ("idEncDecTimeFrame",  property idEncDecTimeFrame)
  , ("idEncDecValueFrame", property idEncDecValueFrame) ]

-- | The two functions from/toBools must form an identity when composed.
idFromToBools :: Word64
              -> Bool
idFromToBools x = x == fromBools (toBools x)

-- | The two functions to/fromBools must form an identity when composed.
idToFromBools :: Property
idToFromBools = forAll (vector 64) $ \xs ->
                xs == toBools (fromBools xs :: Word64)

-- | The two functions pack/unpackBits must form an identity when composed.
idPackUnpackBits :: [Bool]
                 -> Bool
idPackUnpackBits xs = xs == take (length xs) (unpackBits (packBits xs))

-- | The two functions unpack/packBits must form an identity when composed.
idUnpackPackBits :: B.ByteString
                 -> Bool
idUnpackPackBits bs = bs == (packBits (unpackBits bs))

-- | The two functions timeDecode/Encode must form an identity when
-- composed.
idEncDecTimeFrame :: [Word32]
                  -> Bool
idEncDecTimeFrame xs = let ys = reverse $ nub $ sort xs in
                       ys == timeDecode (timeEncode ys)

-- | The two functions valueDecode/Encode must form an identity when
-- composed.
idEncDecValueFrame :: [Float]
                   -> Bool
idEncDecValueFrame xs = xs == valueDecode (valueEncode xs)

