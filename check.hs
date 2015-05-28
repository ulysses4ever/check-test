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

import qualified Filesystem.Path.CurrentOS as FSP -- cabal install system-filepath

-- ********* Checker parameters

taskFilePrefix = "task-"
taskFileSuffix = ".s"
taskCount = 4
buildCmd = "as88"
runCmd = "s88 " -- mind the trailing space
buildFailText = "nerrors"
testDataFilename = "testdata.txt"

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
    ifThenElse OK (FailWithResult aRes) <$> checkResult varId taskId aRes

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

ifThenElse :: a -> a -> Bool -> a
ifThenElse a1 a2 b = if b then a1 else a2

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

-- *** Pretty-printing helper functions
taskVarStamp t v = "task " ++ (t : " (var " ++ [v] ++ "): ")

intersperseWithTwoNl = concat . intersperse "\n\n"

showFileReportList :: [FileReport] -> String
showFileReportList = intersperseWithTwoNl . addTabs . map show

showStudentReportList :: [StudentReport] -> String
showStudentReportList = intersperseWithTwoNl . map show

addTabs = map ("\t" ++ )

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

