-- | Main module with business logic: perform checking of student's submissions
module Checker (studentDirReport) where

import Data.Bool  (bool)
import Data.List  (isInfixOf, sort)
import Data.Maybe (fromJust)

import System.IO (hGetContents)
import System.Process (createProcess, shell, StdStream( CreatePipe )
                      , std_out, std_err)

import Types
import AuxChecker
import AuxFilePath
import CheckerConfig

studentDirReport :: FilePath -> IO StudentReport
studentDirReport studentDir = do
    studentDirContents        <- getAbsDirectoryContents studentDir
    let taskFiles             =  filter isLikeTaskFile $ sort studentDirContents
    fileReports               <- mapM fileReport taskFiles
    return                    $  StudentReport (filename studentDir) fileReports

fileReport :: FilePath -> IO FileReport
fileReport file = do
    maybeVar                        <- getVarFrom file
    let taskId                      =  taskNumFrom file
    let cmd                         =  runCmdFor file
    (_, Just hout, Just herr, _)    <-
        createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
    results                         <- hGetContents hout
    errors                          <- hGetContents herr
    fContents                       <- readFile file
    maybe
        (return $ NoVar taskId)
        (\varId -> FileReport varId taskId <$>
                    checkTask varId taskId fContents errors results)
        maybeVar

checkTask :: VariantId -> TaskId -> String -> String -> String -> IO CheckResult
checkTask varId  taskId taskFileText errors actualRes
    | buildFailText `isInfixOf` errors        = return BuildFail
    | illegalSource varId taskId taskFileText = return IllegalSource
    | otherwise                               =
        checkTaskFallback varId taskId actualRes

-- works when build and source text OK
checkTaskFallback :: VariantId -> TaskId -> String -> IO CheckResult
checkTaskFallback varId taskId aRes =
    bool OK (FailWithResult aRes) <$> checkResult varId taskId aRes

checkResult :: VariantId -> TaskId -> String -> IO Bool
checkResult varId taskId aRes =
    (`elem` words aRes) 
    . fromJust 
    . (lookup (varId, taskId)) <$> (loadTestData defaultTestDataFilename)

illegalSource :: VariantId -> TaskId -> String -> Bool
illegalSource varNum taskNum taskFileText =
    case (varNum, taskNum) `lookup` addChecks of
        Nothing  -> False
        Just chk -> chk taskFileText

