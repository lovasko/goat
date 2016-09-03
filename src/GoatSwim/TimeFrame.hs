module GoatSwim.TimeFrame
( TimeFrame(..) -- ^ *
, timeEncode    -- ^ [Word32] -> TimeFrame
, timeDecode    -- ^ TimeFrame -> [Word32]
) where

import GoatSwim.TimeFrame.Types
import GoatSwim.TimeFrame.Encode
import GoatSwim.TimeFrame.Decode

