import Data.Int
import Data.List
import Data.Word
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

import GoatSwim.TimeFrame
import GoatSwim.TimeFrame.Number
import GoatSwim.Util
import GoatSwim.ValueFrame

-- | The result of the alignTo function must be a list
prop_alignTo :: [Bool]
             -> Bool
prop_alignTo xs = mod (length $ alignTo 8 False xs) 8 == 0

-- | The two functions to/fromBools must form an identity when composed.
prop_idToFromBools :: Property
prop_idToFromBools = forAll (vector 64) $ \bools ->
                     bools == toBools (fromBools bools :: Word64)

-- | The two functions from/toBools must form an identity when composed.
prop_idFromToBools :: Word64
                   -> Bool
prop_idFromToBools word = word == fromBools (toBools word)

-- | Two different words should always map to two different bit lists and two
-- equal words should always map to equal lists.
prop_uniqueToBools :: Word16
                   -> Word16
                   -> Bool
prop_uniqueToBools x y
  | x == y    = toBools x == toBools y
  | otherwise = toBools x /= toBools y

-- | The two functions un/packBits must form an identity when composed.
-- The only difference is that the packBits function aligns the input
-- to the upper-multiply of eight.
prop_idPackUnpackBits :: [Bool]
                    -> Bool
prop_idPackUnpackBits xs = alignTo 8 False xs == unpackBits (packBits xs)

-- | The two functions encode/decodeNumber must form an identity when
-- composed. The test asserts the condition that at least one of the
-- number's bits is valid.
prop_idEncDecNumber :: Int
                    -> Int64
                    -> Property
prop_idEncDecNumber len n = len >= 3 ==>
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
prop_idEncDecTimeFrame :: [Word32]
                       -> Bool
prop_idEncDecTimeFrame xs = let ys = nub $ sort xs in
                            ys == timeDecode (timeEncode (reverse ys))

-- | The two functions valueDecode/Encode must form an identity when
-- composed.
prop_idEncDecValueFrame :: [Float]
                        -> Bool
prop_idEncDecValueFrame xs = xs == valueDecode (valueEncode xs)

-- | Print a name of the property test and execute the QuickCheck
-- algorithm.
runTest :: (String, Property)
        -> IO Result 
runTest (name, prop) =  putStr (name ++ " ")
                     >> quickCheckWithResult args prop
  where
    args = stdArgs {maxSuccess=5000, maxDiscardRatio=10000}

-- | Run all available tests and collect results.
-- TODO rename tests
-- TODO add more unique tests
-- TODO separate ID and unique tests
runTests :: IO [Result]
runTests = mapM runTest tests
  where
    tests = [ ("alignTo           ", property prop_alignTo)
            , ("idToFromBools     ", property prop_idToFromBools)
            , ("idFromToBools     ", property prop_idFromToBools)
            , ("uniqueToBools     ", property prop_uniqueToBools)
            , ("idPackUnpackBits  ", property prop_idPackUnpackBits)
            , ("idEncDecNumber    ", property prop_idEncDecNumber)
            , ("idEncDecTimeFrame ", property prop_idEncDecTimeFrame)
            , ("idEncDecValueFrame", property prop_idEncDecValueFrame)]

-- | Evaluate test results and set appropriate process exit code.
main :: IO ()
main = do
  putStrLn ""
  putStrLn "Running property tests:"
  results <- runTests
  bool exitSuccess exitFailure (all isSuccess results)

