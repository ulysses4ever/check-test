module Main where

import Control.Monad (filterM)
import Data.List (sort)

-- Our modules

import Types (WorkingDirReport(WorkingDirReport))
import AuxFilePath
import Checker

main = do
    workingDir             <- parseArgs
    workingDirContents     <- getAbsDirectoryContents workingDir
    studentDirs            <- filterM isDir $ sort workingDirContents
    reports                <- mapM studentDirReport studentDirs
    writeFile "report.txt" $ show $ WorkingDirReport reports

