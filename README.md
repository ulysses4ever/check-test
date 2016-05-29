## `check-test` — remove groove from checking students' submissions

Basic idea of the tool is to automate process of building and running student submissions. This can be thought of as an initial phase of the actual grading of students' submissions.

There is also a rudimentary mechanism to check output of the programs against some reference results.

The result of the checking is `report.txt` created upon completion.

### Usage

**tl;dr** run `check-test` from the directory with student submissions (they should be placed in subdirs), `testdata.txt` (see below) should be in the same dir.

There are two main dynamic parameters of the checker so far: 

* working directory and 
* file with reference data.

The former one can be supplied as a command-line argument, the default value is `'.'`. Working directory is supposed to contain subdirectories for each student. Each subdir contains student submission.

File with reference data is hardcoded so far: it is a file `testdata.txt` in the current directory. A sample of this file [is here](https://github.com/ulysses4ever/check-test/blob/master/util/sample-testdata.txt).


