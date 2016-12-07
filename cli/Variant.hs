{-# LANGUAGE OverloadedStrings #-}

module Variant
( Variant(..)
, varIdentify
) where

import qualified Data.Text as T
import qualified System.FilePath as F

-- | GoatSwim files come in two formats: raw and compressed.
data Variant = VarZip -- ^ compressed content
             | VarRaw -- ^ uncompressed content

-- | Deduce the variant of a file based on its file extension.
varIdentify :: FilePath              -- ^ file name
            -> Either T.Text Variant -- ^ variant
varIdentify path
  | ext == ".gs"  = Right VarRaw
  | ext == ".gsz" = Right VarZip
  | otherwise     = Left "unrecognized file extension"
  where ext = F.takeExtension path
