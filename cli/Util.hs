module Util
( eitherFirst
, showTime
) where

import Data.Word
import Data.Time.Format
import Data.Time.Clock.POSIX
import qualified Data.Text as T

-- | Apply function to the Left constructor of an Either.
eitherFirst :: (a -> x)   -- ^ function
            -> Either a b -- ^ old either
            -> Either x b -- ^ new either
eitherFirst f (Left a)  = Left (f a)
eitherFirst _ (Right b) = Right b

-- | Convert a timestamp to a human-readable form.
showTime :: Word32 -- ^ time stamp
         -> T.Text -- ^ textual representation
showTime ts = T.pack $ formatTime defaultTimeLocale "%F %T" ts'
  where ts' = posixSecondsToUTCTime $ fromIntegral ts
