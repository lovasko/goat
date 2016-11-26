module Util
( fixTime
) where

import Data.List
import Data.Word

-- | Prepare the random time values into a form expected by the
-- timeEncode function.
fixTime :: [Word32]
        -> [Word32]
fixTime = reverse . sort . nub
