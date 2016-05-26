-- |File system and IO helper functions
module AuxFilePath where

-- you need `cabal install system-filepath` so far
import qualified Filesystem.Path.CurrentOS as FSP 

import System.FilePath ((</>))
import System.Environment (getArgs)
import System.Directory (getDirectoryContents, getPermissions, searchable,
                            canonicalizePath)


parseArgs :: IO String
parseArgs = do
    args <- getArgs
    return $ case args of
        []           -> "."
        [workingDir] -> workingDir

isDir :: FilePath -> IO Bool
isDir p = searchable <$> getPermissions p

getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir =
    getDirectoryContents dir >>=
        mapM (canonicalizePath . (dir </>)) . filter (not .(`elem` [".", ".."]))

filename :: FilePath -> String
filename = FSP.encodeString . FSP.filename . FSP.decodeString

dir :: FilePath -> String
dir = FSP.encodeString . FSP.directory . FSP.decodeString


