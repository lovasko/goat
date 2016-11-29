{-# LANGUAGE OverloadedStrings #-}

module Variant
( Variant(..)
, varIdentify
) where

import qualified Data.Text as T
import qualified System.FilePath as F

-- | GoatSwim files come in two versions: compressed and normal.
data Variant = VarCompressed -- ^ compressed content
             | VarNormal     -- ^ uncompressed content

-- | Deduce the variant of a file based on its file extension.
varIdentify :: FilePath              -- ^ file name
            -> Either T.Text Variant -- ^ variant
varIdentify path
  | ext == ".gs"  = Right VarNormal
  | ext == ".gsc" = Right VarCompressed
  | otherwise     = Left "unrecognized file extension"
  where ext = F.takeExtension path
