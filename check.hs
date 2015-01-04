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
buildCmd = "as88"
runCmd = "s88 " -- mind the trailing space
buildFailText = "nerrors"

-- ********** Main domain types

type VariantId = Char

type TaskId = Char

data FileReport = NoVar TaskId | FileReport VariantId TaskId CheckResult

data CheckResult = BuildFail | IllegalSource | OK | FailWithResult String 

type StudentTag = String

data StudentReport = StudentReport StudentTag [FileReport]

newtype WorkingDirReport = WorkingDirReport [StudentReport]

--  *** Instances for domain types

instance Show FileReport where
    show (NoVar taskId) = "task " ++ [taskId] ++ ": No Variant"
    show (FileReport varId taskId checkRes) =
        taskVarStamp taskId varId ++ show checkRes

instance Show StudentReport where
    show (StudentReport tag rs)  = tag ++ "\n" ++
        case rs of
            [] -> "\tUnrecognized task files..."
            _  -> showFileReportList rs

instance Show CheckResult where
    show BuildFail     = "FAIL to build"
    show IllegalSource = "FAIL to pass source text checks"
    show OK            = "OK"
    show (FailWithResult actualRes) = 
                         "FAIL, actual result: " ++ actualRes

instance Show WorkingDirReport where
    show (WorkingDirReport studReports) = showStudentReportList studReports

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
    let taskNum                     =  taskNumFrom file
    let cmd                         =  runCmdFor file
    (_, Just hout, Just herr, _)    <-
        createProcess (shell cmd) { std_out = CreatePipe, std_err = CreatePipe }
    results                         <- hGetContents hout
    errors                          <- hGetContents herr
    fContents                       <- readFile file
    return                          $  case maybeVar of
        Nothing     -> NoVar taskNum
        Just varNum -> FileReport varNum taskNum $
                            checkTask varNum taskNum fContents errors results

checkTask :: VariantId -> TaskId -> String -> String -> String -> CheckResult
checkTask varNum taskNum taskFileText errors actualRes 
    | buildFailText `isInfixOf` errors          = BuildFail
    | illegalSource varNum taskNum taskFileText = IllegalSource
    | checkResult varNum taskNum actualRes      = OK
    | otherwise                                 = FailWithResult actualRes

checkResult :: VariantId -> TaskId -> String -> Bool
checkResult varNum taskNum actualRes = 
        (fromJust $ (varNum, taskNum) `lookup` res) `elem` -- == ???
            words actualRes

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

runCmdFor file = "cd " ++ dir file ++ 
            " ;  " ++ buildCmd ++ " " ++ filename file ++ 
            " && " ++ runCmd ++ filename file

getVar :: String -> Maybe Char
getVar = find (\c -> c == '1' || c == '2') . head . lines

taskNumFrom :: FilePath -> Char
taskNumFrom = fromJust . find isDigit . filename

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

-- *** Pretty-printing helper functions
taskVarStamp t v = "task " ++ (t : " (var " ++ [v] ++ "): ")

intersperseWithTwoNl = concat . intersperse "\n\n"

showFileReportList :: [FileReport] -> String
showFileReportList = intersperseWithTwoNl . addTabs . map show

showStudentReportList :: [StudentReport] -> String
showStudentReportList = intersperseWithTwoNl . map show

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

