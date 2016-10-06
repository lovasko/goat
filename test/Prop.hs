import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

import Prop.Fluid
import Prop.Id
import Prop.Unique

import GoatSwim.Util

-- | Print a name of the property test and execute the QuickCheck
-- algorithm.
runTest :: (String, Property)
        -> IO Result 
runTest (name, prop) = do
  result <- quickCheckWithResult args prop
  putStr $ unwords [name, output result]
  return result
    where
      args = stdArgs { maxSuccess=50000
                     , maxDiscardRatio=10000
                     , chatty=False }

-- | Run all available property tests and collect results.
runTests :: IO [Result]
runTests = mapM runTest (idProps ++ uniqueProps ++ fluidProps)
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

