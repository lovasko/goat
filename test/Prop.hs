import Data.Int
import Data.List
import Data.Word
import Test.QuickCheck
import Test.QuickCheck.Test

import GoatSwim.Util
import GoatSwim.TimeFrame
import GoatSwim.TimeFrame.Number
import System.Exit

-- | The result of the alignTo function must be a list
prop_alignTo :: [Bool]
             -> Bool
prop_alignTo xs = mod (length $ alignTo 8 False xs) 8 == 0

-- | The two functions to/fromBools must form an identity when composed.
prop_toFromBools :: [Bool]
                 -> Property
prop_toFromBools bools = length bools == 8 ==>
                         bools == toBools (fromBools bools :: Word8)

-- | The two functions from/toBools must form an identity when composed.
prop_fromToBools :: Word64
                 -> Bool
prop_fromToBools word = word == fromBools (toBools word)

-- | The two functions un/packBits must form an identity when composed.
-- The only difference is that the packBits function aligns the input
-- to the upper-multiply of eight.
prop_packUnpackBits :: [Bool]
                    -> Bool
prop_packUnpackBits xs = alignTo 8 False xs == unpackBits (packBits xs)

-- | The two functions encode/decodeNumber must form an identity when
-- composed. The test asserts the condition that at least one of the
-- number's bits is valid.
prop_encdecNumber :: Int
                  -> Int64
                  -> Property
prop_encdecNumber len n = len >= 3 ==>
                          inBounds lo hi n ==>
                          n == decodeNumber len (encodeNumber len n)
  where
    lo = -2^(len-1)
    hi =  2^(len-1)-1

-- | The two function timeDecode/Encode must form an identity when
-- composed.  The test asserts two conditions of the timeEncode function:
-- non-empty list that does not start with zero. The trivial case of an
-- empty list is explicitely handled in the pattern-matching clauses of
-- the functions. A list starting with zero would get interpreted as a
-- empty list by the Encode function and therefore is not allowed.
prop_encdecTimeFrame :: [Word32]
                     -> Property
prop_encdecTimeFrame xs = let ys = nub $ sort xs in
                          not (null xs) ==>
                          head ys /= 0  ==>
                          ys == timeDecode (timeEncode (reverse ys))

-- | Print a name of the property test and execute the QuickCheck
-- algorithm.
runTest :: (String, Property)
        -> IO Result 
runTest (name, prop) =  putStr (name ++ " ")
                     >> quickCheckWithResult args prop
  where
    args = stdArgs {maxSuccess=5000, maxDiscardRatio=10000}

-- | Run all available tests and collect results.
runTests :: IO [Result]
runTests = mapM runTest tests
  where
    tests = [ ("alignTo        ", property prop_alignTo)
            , ("toFromBools    ", property prop_toFromBools)
            , ("fromToBools    ", property prop_fromToBools)
            , ("packUnpackBits ", property prop_packUnpackBits)
            , ("encdecNumber   ", property prop_encdecNumber)
            , ("encdecTimeFrame", property prop_encdecTimeFrame)]

-- | Evaluate test results and set appropriate process exit code.
main :: IO ()
main = do
  putStrLn ""
  putStrLn "Running property tests:"
  results <- runTests
  bool exitSuccess exitFailure (all isSuccess results)

