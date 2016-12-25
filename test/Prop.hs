import Safe
import System.Environment
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

import Prop.Fluid
import Prop.Id
import Prop.Unique

import Codec.Goat.Util

-- | Print a name of the property test and execute the QuickCheck
-- algorithm.
runTest :: Args
        -> (String, Property)
        -> IO Result 
runTest args (name, prop) = do
  result <- quickCheckWithResult args prop
  putStr $ unwords [name, output result]
  return result

-- | Run all available property tests and collect results.
runTests :: Args
         -> IO [Result]
runTests args = mapM (runTest args) (idProps ++ uniqueProps ++ fluidProps)

-- | Parse command-line options into test arguments. In case invalid or
-- no arguments were provided, the test fallbacks into a default value.
parseArguments :: [String]
               -> Args
parseArguments []    = stdArgs { maxSuccess=1000,           chatty=False }
parseArguments (x:_) = stdArgs { maxSuccess=readDef 1000 x, chatty=False }

-- | Evaluate test results and set appropriate process exit code.
main :: IO ()
main = do
  putStrLn "\nRunning property tests:"
  args    <- getArgs
  results <- runTests (parseArguments args)
  bool exitSuccess exitFailure (all isSuccess results)

