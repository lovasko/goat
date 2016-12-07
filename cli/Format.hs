{-# LANGUAGE OverloadedStrings #-}

module Format
( Format(..)
, fmtIdentify
) where

import qualified Data.Text as T
import qualified System.FilePath as F

-- | GoatSwim files come in two formats: raw and compressed.
data Format = FmtZip -- ^ compressed content
            | FmtRaw -- ^ uncompressed content

-- | Deduce the format of a file based on its file extension.
fmtIdentify :: FilePath             -- ^ file name
            -> Either T.Text Format -- ^ error | format
fmtIdentify path
  | ext == ".gs"  = Right FmtRaw
  | ext == ".gsz" = Right FmtZip
  | otherwise     = Left "unrecognized file extension"
  where ext = F.takeExtension path
