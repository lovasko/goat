module Command.Info.Options
( InfoOptions(..)
, infoOptDefine
) where

import Options.Applicative


-- | Options of the "info" command.
data InfoOptions = InfoOptions
  { infoOptStats     :: Bool
  , infoOptTimestamp :: Bool
  , infoOptFile      :: FilePath }

-- | Switch to trigger a computation of value statistics.
optionStats :: Parser Bool -- ^ parser
optionStats = switch
   $ short 's'
  <> long  "stats"
  <> help  "Compute statistics of data point values"

-- | Switch to trigger timestamps instead of formatted dates.
optionTimestamp :: Parser Bool -- ^ parser
optionTimestamp = switch
   $ short 't'
  <> long  "timestamp"
  <> help  "Display data point times as timestamps"

-- | Data file to render.
optionFile :: Parser FilePath -- ^ parser
optionFile = argument str (metavar "FILE")

-- | Command-line user interface.
infoOptDefine :: Parser InfoOptions -- ^ parser
infoOptDefine = InfoOptions <$> optionStats <*> optionTimestamp <*> optionFile
