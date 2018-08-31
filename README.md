# OFC - Open Fortran Compiler

[![Build Status](https://travis-ci.org/CodethinkLabs/ofc.svg?branch=master)](https://travis-ci.org/CodethinkLabs/ofc)

## Overview

Currently OFC is a FORTRAN front-end capable of parsing and performing semantic
analysis on Fortran. We're targeting legacy FORTRAN first, and can currently
parse and semantically analyse most F77 and earlier, providing syntax and
semantic warnings and errors.

### Goals

1. Produce a FORTRAN static code analysis tool.
2. Produce a FORTRAN modernizer which can reprint legacy FORTRAN as F90 where possible.
3. Produce an alternative FORTRAN front-end for open-source compilers.

### Features

- Parse, semantically analyse and re-print ALL NIST F77 tests.
- Safe translation from F77 to F90 syntax where possible.


## Usage
### Cloning
It's recommended that you clone ofc recursively, as it has a submodule:

    git clone https://github.com/CodethinkLabs/ofc.git --recurse-submodules

For git 2.12 or earlier use `--recursive` instead

### Installation
To install ofc, do:

    make install

Note that the ofc binary can be invoked locally without an install,
and the tests will run using the locally built binary.

### Execution
To invoke ofc currently, simply run it over a fortran file:

    ofc tests/programs/nist/FM001.FOR

The compiler options will be automatically detected from the file name, but
this can be overridden using commandline flags which are printed by invoking
ofc with no source file.

To print the parse and semantic trees, use the --parse-tree and --sema-tree flags.


## Testing

### Dependencies

Test Suite:
    gfortran
    valgrind

Static Code Analysis:
	cppcheck
	clang, scanbuild

### Test Suite
We run a test suite including the NIST F77 tests using:

    make test

To make a html report (tests/out/report.html) use:

    make test-report

The full report takes a long time to build as it runs valgrind over both the debug and optimized binaries.
To make a quick report which does every step except valgrind, use:

    make test-report-lite

Note: Tests run from the build directory will use the built ofc rather than the installed one.

### CPPCheck
We run cppcheck over the tree using:

    make cppcheck

### scanbuild
We run scanbuild over the tree using:

    make scan-build

### Other
We also have a rule to compile all files at once to produce more warnings using:

    make scan


## Contact

You can contact us at #open-fortran-compiler on irc.freenode.net or via the e-mails used in our git commits.
