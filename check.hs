import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Directory (getDirectoryContents, getPermissions, searchable, 
                            canonicalizePath)
import System.IO
import System.Process

import Control.Monad (filterM, (>=>))

import Data.List
import Data.Char
import Data.Maybe

import qualified Filesystem.Path.CurrentOS as FSP -- cabal install system-filepath

-- ********* Checker parameters

taskFilePrefix = "task-"
taskFileSuffix = ".s"
taskCount = 4
taskFileNames = [taskFilePrefix ++ (show i) ++ taskFileSuffix | 
                                                            i <- [1..taskCount]]
runCmdFor file = "cd " ++ dir file ++ 
            " ;  as88 " ++ filename file ++ 
            " && s88 "  ++ filename file

-- ********** Main domain types and functions

type VariantId = Char
type TaskId = Char

main = do
    workingDir             <- parseArgs
    workingDirContents     <- getAbsDirectoryContents workingDir
    studentDirs            <- filterM isDir $ sort workingDirContents
    reports                <- mapM reportStudentDir studentDirs
    writeFile "report.txt" (concatIndividualReports reports)

reportStudentDir :: FilePath -> IO String
reportStudentDir studentDir = do
    studentDirContents        <- getAbsDirectoryContents studentDir
    let taskFiles             =  filter isLikeTaskFile $ sort studentDirContents
    fileReports               <- mapM reportFile taskFiles
    return $ filename studentDir  ++ "\n" ++ 
        if null taskFiles then "\tUnrecognized task files..."
        else concatIndividualFileReports fileReports

isLikeTaskFile :: FilePath -> Bool
isLikeTaskFile file = let
        fStr = filename file
    in any (fStr `isSuffixOf`) taskFileNames

reportFile :: FilePath -> IO String
reportFile file = do
    maybeVar                        <- getVarFrom file
    let taskNum                     = taskNumFrom file
    let cmd                         = runCmdFor file
    (_, Just hout, Just herr, _)    <-
        createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
    results                         <- hGetContents hout
    errors                          <- hGetContents herr
    fContents                       <- readFile file
    return $ case maybeVar of
        Nothing     -> "No var"
        Just varNum -> taskVarStamp taskNum varNum ++
            checkTask varNum taskNum fContents errors results

getVarFrom :: FilePath -> IO (Maybe VariantId)
getVarFrom file = readFile file >>= return . getVar

getVar :: String -> Maybe Char
getVar = find (\c -> c == '1' || c == '2') . head . lines

taskNumFrom :: FilePath -> Char
taskNumFrom = fromJust . find isDigit . filename

checkTask :: VariantId -> TaskId -> String -> String -> String -> String
checkTask varNum taskNum taskFileText errors actualRes 
    | elem "nerrors" (words errors) = 
            "FAIL to assebmle\n" -- : unlines $ "FAIL":(addTabs $ lines actualRes)
    | illegalText varNum taskNum taskFileText =
            " FAIL to pass text checks\n"
    | checkResult varNum taskNum actualRes =
            " OK\n"
    | otherwise = 
            " FAIL, actual result: " ++ actualRes ++ "\n"

checkResult :: VariantId -> TaskId -> String -> Bool
checkResult varNum taskNum actualRes = 
        (fromJust $ (varNum, taskNum) `lookup` res) `elem` 
            words actualRes -- == ???

illegalText :: VariantId -> TaskId -> String -> Bool
illegalText varNum taskNum taskFileText = 
    case (varNum, taskNum) `lookup` addChecks of
        Nothing  -> False
        Just chk -> chk taskFileText

-- ******* Small helper functions

taskVarStamp t v = "task " ++ (t : " (var " ++ [v] ++ "): ")

composeM :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
composeM fM g = fM >=> (return . g)

isDir :: FilePath -> IO Bool
isDir = composeM getPermissions searchable

parseArgs :: IO String
parseArgs = do 
    args <- getArgs
    return $ case args of
        []           -> "."
        [workingDir] -> workingDir

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir =
    getDirectoryContents dir >>= 
        mapM (canonicalizePath . (dir </>)) . filter (not .(`elem` [".", ".."])) 

filename :: FilePath -> String
filename = FSP.encodeString . FSP.filename . FSP.decodeString

dir :: FilePath -> String
dir = FSP.encodeString . FSP.directory . FSP.decodeString

concatIndividualFileReports :: [String] -> String
concatIndividualFileReports = concat . intersperse "\n" . addTabs

concatIndividualReports :: [String] -> String
concatIndividualReports = concat . intersperse "\n\n"

addTabs = map ("\t" ++ )

-- ******* Reference results
res = [
        (('1', '1'), "16"),
        (('1', '2'), "9"),
        (('1', '3'), "0"),
        (('1', '4'), "1"),
        (('2', '1'), "20"),
        (('2', '2'), "3"),
        (('2', '3'), "0"),
        (('2', '4'), "1")
    ]

-- ******* Extra checks for solutions
hasWord word = (word `elem`) . words . (map toUpper)

hasDiv = hasWord "DIV"
hasRep = hasWord "REP"

addChecks = [
        (('1', '1'), hasDiv),
        (('1', '3'), hasRep),
        (('2', '2'), hasDiv),
        (('2', '4'), hasRep)
    ]

