-- | Main domain types for checking tests
module Types 
             (VariantId
             , TaskId
             , CheckResult(BuildFail, IllegalSource, OK, FailWithResult)
             , FileReport(NoVar, FileReport)
             , WorkingDirReport(WorkingDirReport)
             , StudentReport(StudentReport)
             , StudentTag) where

import Data.List (intersperse)

-- | Report on directory with many student's sudmission
newtype WorkingDirReport = WorkingDirReport [StudentReport]

-- | Report on a whole student's sudmission
data StudentReport = StudentReport StudentTag [FileReport]

-- | Report on one file (solution for one task) from a student's submission
data FileReport = NoVar TaskId | FileReport VariantId TaskId CheckResult

-- | Some tag to identify a student (usually, holding their name)
type StudentTag = String

-- | A type for coding a variant of a test
type VariantId = Char

-- | A type for coding task id
type TaskId = Char

-- | Datatype representing result of checking a solution 
--   from student's submission
data CheckResult = BuildFail | IllegalSource | OK | FailWithResult String

--  *** Showing the types

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

-- *** Pretty-printing helper functions

taskVarStamp t v = "task " ++ (t : " (var " ++ [v] ++ "): ")

intersperseWithTwoNl = concat . intersperse "\n\n"

showFileReportList :: [FileReport] -> String
showFileReportList = intersperseWithTwoNl . addTabs . map show

showStudentReportList :: [StudentReport] -> String
showStudentReportList = intersperseWithTwoNl . map show

addTabs = map ("\t" ++ )

