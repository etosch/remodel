remodel
=======

691ST  Project 2 - make->remodel

Usage
----
After cloning, run `build.sh`. 	`build.sh` performs the following steps:

This will create a REMODELFILE in the current directory that will be used for testing. The following executables must be accessible from the current directory: `ocamlbuild`, `md5`, `g++`. Note that the `md5` call is used to perform the tests and is not used in the remodel executable.

Each of the four tests is separated by a sleep command. This allows more dynamic testing conditions. 

`remodel` is executed in the REMODELFILE's directory and takes zero or more target names.

Freshness and Parallelism
----
`remodel` checks for freshness in several places. At the beginning of a build, it loads in the MD5 hashes of its dependencies. The program traverses the dependency tree recursively, forking processes for each each dependency from a target. The target waits on the children to return before invoking its command.

Before executing a command, the current process first checks whether the old MD5 hashes of both the target file and its dependencies are consistent with the current hash of each file. If the check is consistent, it skips executing this command and returns to its calling parent. If the check is not consistent, then it needs to perform another check - it needs to look at the log to see if another processs has already executed this command. It first scans the log directory for files having the target name in the file name. If it does not find any, then it executes its command and logs this file. If it finds files with the target's filename, it then reads each of them, checking whether the same command has already been executed. If the same command has already been executed for this target by another process, it does not execute its command and returns to the parent. If the command has not been executed, then it executes its command and logs this appropriately.

Known Problems
----
MD5 hashes for files are stored in the same order that they are entered in the REMODELFILE. They are not tagged with their filename, so they cannot detect when their recorded order changes. A future implementation would sort the files lexicographically before hashing. 

There is the potential for race conditions between multiple processes operating on the same file and recording their status to the `.remodel/logdone` directory. However, since each command is in a file named by the target's name and the pid of the processes executing the command within, such scenarios can be detected post-build.

The REMODEL file cannot handle nested double quotations in commands.

The graph type is not currently used. I had wanted to do something like check the md5 of the remodel file. Not sure this is worth doing.