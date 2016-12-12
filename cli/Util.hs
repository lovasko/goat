module Util
( eitherFirst
, showTime
, textShow
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

-- | Convert any Showable type into a Text instance.
textShow :: (Show a)
         => a      -- ^ any type
         -> T.Text -- ^ textual representation
textShow = (T.pack . show)

-- | If/else construct.
bool :: a    -- ^ True option
     -> a    -- ^ False option
     -> Bool -- ^ bool
     -> a    -- ^ result
bool x _ True  = x
bool _ y False = y

-- | Convert a timestamp to a human-readable form.
showTime :: Bool   -- ^ keep in timestamp form
         -> Word32 -- ^ timestamp
         -> T.Text -- ^ textual representation
showTime True  ts = textShow ts
showTime False ts = T.pack (formatTime defaultTimeLocale "%F %T" ts')
  where ts' = posixSecondsToUTCTime (fromIntegral ts)
