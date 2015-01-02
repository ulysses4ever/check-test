import System.IO
import System.Process
import Control.Monad (filterM)
import Data.List
import Data.Char
import Data.Maybe
import qualified Filesystem as FS
import qualified Filesystem.Path.CurrentOS as FSP

taskFilePrefix = "task-"
taskFileSuffix = ".s"
taskCount = 4
taskFileNames = [taskFilePrefix ++ (show i) ++ taskFileSuffix | 
													i <- [1..taskCount]]

main = do
	sdirsRaw <- FS.listDirectory (FSP.decodeString ".")
	studentDirs <- filterM FS.isDirectory sdirsRaw
	ireps <- mapM reportStudentDir studentDirs
	writeFile "report.txt" (concatIndividualReports ireps)

reportStudentDir :: FSP.FilePath -> IO String
reportStudentDir studentDir = do
	studentDirContents <- FS.listDirectory studentDir
	let taskFiles = filter isLikeTaskFile studentDirContents
	fileReports <- mapM reportFile taskFiles
	return $ FSP.encodeString studentDir  ++ "\n" ++ 
		if null taskFiles then "\tUnrecognized task files..."
		else concatIndividualFileReports fileReports

isLikeTaskFile file = let
	fStr = FSP.encodeString $ FSP.filename file
	in any (fStr `isSuffixOf`) taskFileNames	

reportFile :: FSP.FilePath -> IO String
reportFile file = do
	let taskNum = getTaskNum file
	let cmd = runCmd file
	(_, Just hout, Just herr, _) <-
		createProcess (shell cmd){ std_out = CreatePipe, std_err = CreatePipe }
	results <- hGetContents hout
	errors <- hGetContents herr
	fContents <- readFile $ FSP.encodeString file
	let maybeVar = getVar fContents
	return $ case maybeVar of
		Nothing     -> "No var"
		Just varNum -> taskVarStamp taskNum varNum ++
			checkTask varNum taskNum fContents errors results

taskVarStamp t v = "task " ++ (t : " (var " ++ [v] ++ "): ")

runCmd file = "cd " ++ (FSP.encodeString $ FSP.directory file) ++ 
		" ;  as88 " ++ (FSP.encodeString $ FSP.filename file) ++ 
		" && s88 "  ++ (FSP.encodeString $ FSP.filename file)

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

getTaskNum = fromJust . find isDigit . FSP.encodeString . FSP.filename

getVar = find (\c -> c == '1' || c == '2') . head . lines

concatIndividualFileReports :: [String] -> String
concatIndividualFileReports = concat . intersperse "\n" . addTabs

concatIndividualReports :: [String] -> String
concatIndividualReports = concat . intersperse "\n\n"

addTabs = map ("\t" ++ )

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

-- ******* Text file with solutions checks *********
hasWord word = (word `elem`) . words . (map toUpper)

hasDiv = hasWord "DIV"
hasRep = hasWord "REP"

addChecks = [
		(('1', '1'), hasDiv),
		(('1', '3'), hasRep),
		(('2', '2'), hasDiv),
		(('2', '4'), hasRep)
	]
