import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

import Prop.Id
import Prop.Unique

import GoatSwim.Util

-- | The two functions un/packBits must form an identity when composed.
-- The only difference is that the packBits function aligns the input
-- to the upper-multiply of eight.
--prop_idPackUnpackBits :: [Bool]
--                      -> Bool
--prop_idPackUnpackBits xs = alignTo 8 False xs == unpackBits (packBits xs)

-- | Print a name of the property test and execute the QuickCheck
-- algorithm.
runTest :: (String, Property)
        -> IO Result 
runTest (name, prop) = do
  result <- quickCheckWithResult args prop
  putStr $ unwords [name, output result]
  return result
    where
      args = stdArgs { maxSuccess=100000
                     , maxDiscardRatio=10000
                     , chatty=False }

-- | Run all available property tests and collect results.
runTests :: IO [Result]
runTests = mapM runTest (idProps ++ uniqueProps)
--    tests = [ ("alignTo           ", property prop_alignTo)
--            , ("idToFromBools     ", property prop_idToFromBools)
--            , ("idFromToBools     ", property prop_idFromToBools)
--            , ("uniqueToBools     ", property prop_uniqueToBools)
--            , ("idPackUnpackBits  ", property prop_idPackUnpackBits)
--            , ("idEncDecTimeFrame ", property prop_idEncDecTimeFrame)
--            , ("idEncDecValueFrame", property prop_idEncDecValueFrame)]

-- | Evaluate test results and set appropriate process exit code.
main :: IO ()
main = do
  putStrLn ""
  putStrLn "Running property tests:"
  results <- runTests
  bool exitSuccess exitFailure (all isSuccess results)

