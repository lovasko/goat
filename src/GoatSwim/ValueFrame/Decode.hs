module GoatSwim.ValueFrame.Decode
( valueDecode
) where

import Data.Bits
import Data.Bits.Floating
import Data.List
import Data.List.Split
import Data.Word

import qualified Data.ByteString as B

import GoatSwim.ValueFrame.Types
import GoatSwim.Util

-- | Helper type to hold the decoding context.
type Context = ([Bool], (Int, Int))

-- | Unpack value points from the succinct frame.
valueDecode :: ValueFrame
            -> [Float]
valueDecode (ValueFrame Nothing _ _)       = []
valueDecode (ValueFrame (Just x) len bytes)
  | B.null bytes || len == 0 = [coerceToFloat x]
  | otherwise                = map coerceToFloat (x:xors)
  where
    xors  = zipWith xor words (tail words)  :: [Word32]
    words = unfoldr decode (bits, (12, 12)) :: [Word32]
    bits  = take len (unpackBits bytes)     :: [Bool]

-- | Decode a single XORed value.
decode :: Context
       -> Maybe (Word32, Context)
decode (False:xs,      bounds) = Just (0, (xs, bounds))
decode (True:False:xs, bounds) = Just $ inside bounds xs
decode (True:True:xs,  _     ) = Just $ outside xs
decode (_,             _     ) = Nothing

--decode ((True:False:ys), box@(lead, trail)) = Just (word, (rest, box))
--  where
--    word         = fromListLE number
--    (bits, rest) = splitAt (32-lead-trail)
--    number       = replicate False lead  ++ bits ++ replicate False trail

--decode ((True:True:ys), _) = Just (word, (rest, (lead, trail)))
--  where
--    [lead, size, zs] = splitPlaces ([5, 6, maxBound] :: Int) ys
--    trail            = 32 - lead - (fromListLE size)
--    (bits, rest)     = splitAt (fromListLE size) zs


--    number           = replicate False lead  ++ bits ++ replicate False trail
--    word             = fromListLE number

-- | Handling of the within-bounds decoding case.
inside :: (Int, Int)        -- ^ current bounds
       -> [Bool]            -- ^ bits
       -> (Word32, Context) -- ^ decoded number & context
inside bounds@(lead, trail) xs = (fromBools number, (ys, bounds))
  where
    number     = surround (lead, trail) bits
    (bits, ys) = splitAt (32 - lead - trail) xs

-- | Handling of the out-of-bounds decoding case.
outside :: [Bool]            -- ^ bits
        -> (Word32, Context) -- ^ decoded number & context
outside xs = (fromBools number, (ys, (lead, trail)))
  where
    [lead, size] = map fromBools $ splitPlaces [5, 6] xs
    trail        = 32 - lead - size
    number       = surround (lead, trail) bits
    (bits, ys)   = splitAt size (drop 11 xs)

-- | Surround a list of bools with False elements.
surround :: (Int, Int)
         -> [Bool]
         -> [Bool]
surround (lead, trail) xs = replicate lead False ++ xs ++
                            replicate trail False

