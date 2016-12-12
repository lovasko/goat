{-# LANGUAGE OverloadedStrings #-}

module Command.Show.Perform
( perform
) where

import Data.Monoid
import Data.Time.Format
import System.Exit
import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Load
import Story
import Util


-- | Create a table entry from a story data point.
createEntry :: Bool            -- ^ pretty-print time
            -> (Word32, Float) -- ^ data point
            -> [T.Text]        -- ^ table entry
createEntry form (time, value) = [showTime form time, T.pack (show value)]

-- | Create the final table layout.
createTable :: ShowOptions -- ^ command-line options
            -> Story       -- ^ story
            -> T.Text      -- ^ table layout
createTable options story = tabl EnvAscii hdecor vdecor aligns cells
  where
    hdecor  = DecorUnion [DecorOuter, DecorOnly [1]]
    vdecor  = DecorAll
    aligns  = [AlignLeft, AlignRight]
    cells   = ["Time", "Value"] : (take (showOptCount options) entries)
    entries = map (createEntry (showOptTimestamp options)) story

-- | Pretty-print the content of a story file.
perform :: ShowOptions -- ^ options
        -> IO ()       -- ^ command action
perform options story = do
  result <- storyLoad (showOptFile options)
  case result of
    Left  err   -> T.putStrLn ("ERROR: " <> err)          >> exitFailure
    Right story -> T.putStrLn (createTable story options) >> exitSuccess
