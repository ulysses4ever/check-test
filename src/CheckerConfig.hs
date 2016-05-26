-- |Representation of main Checker parameters
module CheckerConfig where

taskFilePrefix = "task-"
taskFileSuffix = ".s"
taskCount = 4
buildCmd = "as88"
runCmd = "s88 " -- mind the trailing space
buildFailText = "nerrors"
defaultTestDataFilename = "testdata.txt"

