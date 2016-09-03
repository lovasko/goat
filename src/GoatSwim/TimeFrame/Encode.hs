module GoatSwim.TimeFrame.Encode
( timeEncode
) where

import Data.Bits.Bitwise (toListLE)
import Data.Word
import Data.Int
import qualified Data.ByteString as B

import GoatSwim.TimeFrame.Types
import GoatSwim.TimeFrame.Number
import GoatSwim.Util

-- | Pack a list of time points into a succinct frame form.
timeEncode :: [Word32]  -- ^ time points
           -> TimeFrame -- ^ succinct frame form
timeEncode []     = TimeFrame Nothing  Nothing  0 B.empty
timeEncode [x]    = TimeFrame (Just x) Nothing  0 B.empty
timeEncode [x, y] = TimeFrame (Just y) (Just x) 0 B.empty
timeEncode xs     = TimeFrame (Just x) (Just y) (length bits) (packBits bits)
  where
    bits        = concatMap encode dods             :: [Bool]
    dods        = zipWith sub deltas ((y-x):deltas) :: [Int64]
    deltas      = zipWith (-) times (y:times)       :: [Word32]
    (x:y:times) = reverse xs                        :: [Word32]

-- | Encode the new incoming value based on its delta of a delta.
encode :: Int64  -- ^ delta of a delta
       -> [Bool] -- ^ new bits
encode dod
  | dod == 0                  = header 0 1
  | inBounds   (-64)   63 dod = header 1 1 ++ encodeNumber  7 dod
  | inBounds  (-256)  255 dod = header 2 1 ++ encodeNumber  9 dod
  | inBounds (-2048) 2047 dod = header 3 1 ++ encodeNumber 12 dod
  | otherwise                 = header 4 0 ++ toListLE dod
  where
    header t f = replicate t True ++ replicate f False

