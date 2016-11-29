module Story
( Story
) where

import Data.Word

-- | Representation of a value change mapped over time.
type Story = [(Word32, Float)]
