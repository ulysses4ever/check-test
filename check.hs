module Main where

import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Directory (getDirectoryContents, getPermissions, searchable,
                            canonicalizePath)
import System.IO
import System.Process

import Control.Applicative ((<$>))
import Control.Monad (filterM, (>=>))

import Data.List
import Data.Char
import Data.Maybe
import Data.Bool (bool)

import qualified Filesystem.Path.CurrentOS as FSP -- cabal install system-filepath

-- Our modules

import Types

-- ********* Checker parameters

taskFilePrefix = "task-"
taskFileSuffix = ".s"
taskCount = 4
buildCmd = "as88"
runCmd = "s88 " -- mind the trailing space
buildFailText = "nerrors"
testDataFilename = "testdata.txt"

-- ********** Main domain functions

main = do
    workingDir             <- parseArgs
    workingDirContents     <- getAbsDirectoryContents workingDir
    studentDirs            <- filterM isDir $ sort workingDirContents
    reports                <- mapM studentDirReport studentDirs
    writeFile "report.txt" $ show $ WorkingDirReport reports

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
    (`elem` words aRes) . fromJust . (lookup (varId, taskId)) <$> loadTestData

illegalSource :: VariantId -> TaskId -> String -> Bool
illegalSource varNum taskNum taskFileText =
    case (varNum, taskNum) `lookup` addChecks of
        Nothing  -> False
        Just chk -> chk taskFileText

-- ******* Small helper functions

composeM :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
composeM fM g = fM >=> (return . g)

taskFileNames :: [String]
taskFileNames = [taskFilePrefix ++ (show i) ++ taskFileSuffix |
                                                            i <- [1..taskCount]]

isLikeTaskFile :: FilePath -> Bool
isLikeTaskFile file = let
        fStr = filename file
    in any (fStr `isSuffixOf`) taskFileNames

runCmdFor file = "cd '" ++ dir file ++
            "' ;  " ++ buildCmd ++ " " ++ filename file ++
            " && " ++ runCmd ++ filename file

getVar :: String -> Maybe Char
getVar = find (\c -> c == '1' || c == '2') . head . lines

taskNumFrom :: FilePath -> Char
taskNumFrom = fromJust . find isDigit . filename

-- Detect line to be thrown out from config files
isServiceLine :: String -> Bool
isServiceLine line = null line || "--" `isPrefixOf` line

-- *** File system and IO helper functions
parseArgs :: IO String
parseArgs = do
    args <- getArgs
    return $ case args of
        []           -> "."
        [workingDir] -> workingDir

isDir :: FilePath -> IO Bool
isDir = composeM getPermissions searchable

getVarFrom :: FilePath -> IO (Maybe VariantId)
getVarFrom file = readFile file >>= return . getVar

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir =
    getDirectoryContents dir >>=
        mapM (canonicalizePath . (dir </>)) . filter (not .(`elem` [".", ".."]))

filename :: FilePath -> String
filename = FSP.encodeString . FSP.filename . FSP.decodeString

dir :: FilePath -> String
dir = FSP.encodeString . FSP.directory . FSP.decodeString

-- ******* Load reference results

loadTestData :: IO [((VariantId, TaskId), String)]
loadTestData = do
    rawFileText <- readFile testDataFilename
    let ls = filter (not . isServiceLine) $ lines rawFileText
    let lws = map words ls
    return $ zip
        (map ((\[[v], [t]] -> (v, t)) . (take 2)) lws)
        (map (concat . (drop 2)) lws)

-- ******* Extra checks for solutions
hasWord word = (word `elem`) . words . (map toUpper)

hasDiv = hasWord "DIV"
hasRep = hasWord "REP"

addChecks = [
--        (('1', '1'), hasDiv),
--        (('1', '3'), hasRep),
--        (('2', '2'), hasDiv),
--        (('2', '4'), hasRep)
    ]

