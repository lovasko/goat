{-# LANGUAGE OverladedStrings #-}

module Command.Info.Perform
( perform
) where

import System.Exit
import Text.Tabl
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Command.Info.Options
import Load
import Story
import Util


-- | Compute statistics on top of data point values.
valueStats :: [Float]    -- ^ values
           -> [[T.Text]] -- ^ table rows
valueStats [] = []
valueStats xs = [["Min:", (textShow . minimum) xs]
                ,["Max:", (textShow . maximum) xs]]

-- | Compute statistics on top of data point times.
timeStats :: InfoOptions
          -> [Word32]
          -> [[T.Text]]
timeStats _       []    = []
timeStats options times = [["Begin:", timeBegin]
                          ,["End:",   timeEnd]
                          ,["Size:",  entryCount]]
  where
    timeBegin  = showTime (infoOptTimestamp options) (head times)
    timeEnd    = showTime (infoOptTimestamp options) (last times)
    entryCount = (textShow . length) times

-- | Create table rows with information about the data points.
createTable :: InfoOptions
            -> Story
            -> [[T.Text]]
createTable _       []    = [[]]
createTable options story = tabl EnvAscii hdecor vdecor aligns cells
  where
    hdecor = DecorNone
    vdecor = DecorNone
    aligns = repeat AlignRight
    cells  = times ++ bool values [] (infoOptStats options)
    times  = timeStats options (map fst story)
    values = valueStats (map snd story)

-- | Print information about the data points stored in a file.
perform :: InfoOptions -- ^ command-line options
        -> IO ()       -- ^ action
perform options = do
  result <- storyLoad (infoOptFile options)
  case result of
    Left  err   -> T.putStrLn ("ERROR: " <> err)          >> exitFailure
    Right story -> T.putStrLn (createTable options story) >> exitSuccess
