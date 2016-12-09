{-# LANGUAGE OverloadedStrings #-}

module Save
( storySave
) where

import Control.Monad
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.Serialize as S
import qualified Data.Text.IO as T

import GoatSwim.TimeFrame
import GoatSwim.ValueFrame
import Format
import Story


-- | Save a story into a file in the uncompressed format.
saveRaw :: Story        -- ^ story
        -> B.ByteString -- ^ binary encoded story
saveRaw story = S.runPut (mapM_ rule story)
  where rule = S.putTwoOf S.putWord32be S.putFloat32be

-- | Save a story into a file in the compressed format.
saveZip :: Story        -- ^ story
        -> B.ByteString -- ^ binary encoded story
saveZip story = S.runPut (S.put times >> S.put values)
  where
    times  = timeEncode  (map fst story)
    values = valueEncode (map snd story)

-- | Save a story into a file.
storySave :: Story    -- ^ story
          -> FilePath -- ^ file path
          -> IO ()    -- ^ action
storySave story path =
  case fmtIdentify path of
    Left  err    -> T.putStrLn ("ERROR:" <> err)
    Right FmtRaw -> B.writeFile path (saveRaw story)
    Right FmtZip -> B.writeFile path (saveZip story)
