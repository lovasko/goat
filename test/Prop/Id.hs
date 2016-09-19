module Prop.Id
( idProps
) where

import Data.List
import Data.Word
import Test.QuickCheck

import GoatSwim.TimeFrame
import GoatSwim.Util
import GoatSwim.ValueFrame

-- | All tests that assert the fact that encode/decode functions
-- must form an identity when composed.
idProps :: [(String, Property)]
idProps =
  [ ("idToFromBools",      property idToFromBools)
  , ("idFromToBools",      property idFromToBools)
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

