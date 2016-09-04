module GoatSwim.TimeFrame
( TimeFrame(..) -- ^ *
, timeDecode    -- ^ TimeFrame -> [Word32]
, timeEncode    -- ^ [Word32] -> TimeFrame
) where

import GoatSwim.TimeFrame.Decode
import GoatSwim.TimeFrame.Encode
import GoatSwim.TimeFrame.Types

