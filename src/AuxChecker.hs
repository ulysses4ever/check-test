-- | Helper utilities for the Checker
module AuxChecker where

import Data.Char  (toUpper, isDigit)
import Data.List  (isPrefixOf, isSuffixOf, find)
import Data.Maybe (fromJust)

import Types
import AuxFilePath
import CheckerConfig

-- |Load reference results
loadTestData :: FilePath -> IO [((VariantId, TaskId), String)]
loadTestData testDataFilename = do
    rawFileText <- readFile testDataFilename
    let ls = filter (not . isServiceLine) $ lines rawFileText
    let lws = map words ls
    return $ zip
        (map ((\[[v], [t]] -> (v, t)) . (take 2)) lws)
        (map (concat . (drop 2)) lws)

-- |Detect line to be thrown out from config or reference files
isServiceLine :: String -> Bool
isServiceLine line = null line || "--" `isPrefixOf` line

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

getVarFrom :: FilePath -> IO (Maybe VariantId)
getVarFrom file = readFile file >>= return . getVar

-- ******* Extra source checks for student's solutions

hasWord word = (word `elem`) . words . (map toUpper)

hasDiv = hasWord "DIV"
hasRep = hasWord "REP"

addChecks = [
--        (('1', '1'), hasDiv),
--        (('1', '3'), hasRep),
--        (('2', '2'), hasDiv),
--        (('2', '4'), hasRep)
    ]

