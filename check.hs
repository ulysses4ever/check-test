import System.IO
import System.Process
import System.Environment (getArgs)
import System.Directory (getDirectoryContents, getPermissions, searchable, canonicalizePath)
import Control.Monad (filterM, (>=>))
import Data.List
import Data.Char
import Data.Maybe
import qualified Filesystem.Path.CurrentOS as FSP
import System.FilePath ((</>))


taskFilePrefix = "task-"
taskFileSuffix = ".s"
taskCount = 4
taskFileNames = [taskFilePrefix ++ (show i) ++ taskFileSuffix | 
													i <- [1..taskCount]]

main = do
	workingDir 			<- parseArgs
	workingDirContents 	<- getAbsDirectoryContents workingDir
	studentDirs 		<- filterM isDir workingDirContents
	reports 			<- mapM reportStudentDir studentDirs
	writeFile "report.txt" (concatIndividualReports reports)
--	return ()

reportStudentDir :: FilePath -> IO String
reportStudentDir studentDir = do
	studentDirContents <- getDirectoryContents studentDir
	let taskFiles = filter isLikeTaskFile studentDirContents
	fileReports <- mapM reportFile taskFiles
	return $ studentDir  ++ "\n" ++ 
		if null taskFiles then "\tUnrecognized task files..."
		else concatIndividualFileReports fileReports

isLikeTaskFile :: FilePath -> Bool
isLikeTaskFile file = let
	fStr = FSP.encodeString $ FSP.filename $ FSP.decodeString file
	in any (fStr `isSuffixOf`) taskFileNames

reportFile :: FilePath -> IO String
reportFile file = do
	let taskNum = getTaskNum file
	let cmd = runCmd file
	(_, Just hout, Just herr, _) <-
		createProcess (shell cmd){ std_out = CreatePipe, std_err = CreatePipe }
	results <- hGetContents hout
	errors <- hGetContents herr
	fContents <- readFile file
	let maybeVar = getVar fContents
	return $ case maybeVar of
		Nothing     -> "No var"
		Just varNum -> taskVarStamp taskNum varNum ++
			checkTask varNum taskNum fContents errors results

taskVarStamp t v = "task " ++ (t : " (var " ++ [v] ++ "): ")

runCmd file = "cd " ++ dir file ++ 
		" ;  as88 " ++ filename file ++ 
		" && s88 "  ++ filename file

checkTask varNum taskNum taskFileText errors actualRes 
	| elem "nerrors" (words errors) = 
			"FAIL to assebmle\n" -- : unlines $ "FAIL":(addTabs $ lines actualRes)
	| illegalText varNum taskNum taskFileText =
			" FAIL to pass text checks\n"
	| checkResult varNum taskNum actualRes =
			" OK\n"
	| otherwise = 
			" FAIL, actual result: " ++ actualRes ++ "\n"

checkResult varNum taskNum actualRes = 
		(fromJust $ (varNum, taskNum) `lookup` res) `elem` 
			words actualRes -- == ???
		
illegalText varNum taskNum taskFileText = 
	case (varNum, taskNum) `lookup` addChecks of
		Nothing  -> False
		Just chk -> chk taskFileText

getTaskNum = fromJust . find isDigit . filename

getVar = find (\c -> c == '1' || c == '2') . head . lines

-- ******* Small helper functions
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
		mapM (canonicalizePath . (dir </>))

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

-- ******* Extra checks for solutions *********
hasWord word = (word `elem`) . words . (map toUpper)

hasDiv = hasWord "DIV"
hasRep = hasWord "REP"

addChecks = [
		(('1', '1'), hasDiv),
		(('1', '3'), hasRep),
		(('2', '2'), hasDiv),
		(('2', '4'), hasRep)
	]
