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
import Story
import Variant


-- | Save a story into a file in the uncompressed format.
saveNormal :: Story        -- ^ story
           -> B.ByteString -- ^ binary encoded story
saveNormal story = S.runPut (mapM_ rule story)
  where rule = S.putTwoOf S.putWord32be S.putFloat32be

-- | Save a story into a file in the compressed format.
saveCompressed :: Story        -- ^ story
               -> B.ByteString -- ^ binary encoded story
saveCompressed story = S.runPut (S.put times >> S.put values)
  where
    times  = timeEncode  (map fst story)
    values = valueEncode (map snd story)

-- | Save a story into a file.
storySave :: Story    -- ^ story
          -> FilePath -- ^ file path
          -> IO ()    -- ^ action
storySave story path =
  case varIdentify path of
    Left  err           -> T.putStrLn ("ERROR:" <> err)
    Right VarNormal     -> B.writeFile path (saveNormal     story)
    Right VarCompressed -> B.writeFile path (saveCompressed story)
