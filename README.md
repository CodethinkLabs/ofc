# OFC - Open Fortran Compiler


## Overview

Currently OFC is a FORTRAN front-end capable of parsing and performing semantic
analysis on Fortran. We're targeting legacy FORTRAN first, and can currently
parse most F77 and earlier, and provide parsing and semantic errors and
warnings.

### Goals

1. Produce a FORTRAN static code analysis tool.
2. Produce a FORTRAN modernizer which can reprint legacy FORTRAN as F90 where possible.
3. Produce an alternative FORTRAN front-end for open-source compilers.

### Features

- Parse, semantically analyse and re-print ALL NIST F77 tests.
- Safe translation from F77 to F90 syntax where possible.


## Usage

### Installation
To install ofc, do:

    make install

Note that the ofc binary can be invoked locally without an install,
and the tests will run using the locally built binary.

### Execution
To invoke ofc currently, simply run it over a fortran file:

    ofc tests/FM001.FOR

The compiler options will be automatically detected from the file name, but
this can be overridden using commandline flags which are printed by invoking
ofc with no source file.

To print the parse and semantic trees, use the --parse-tree and --sema-tree flags.


## Testing

### Test Suite
We run a test suite including the NIST F77 tests using:

    make tests

Not all of these tests currently pass semantic analysis, we're working on this.

### Valgrind
We run valgrind over both the debug and optimized binaries with:

    make valgrind
    make valgrind-optimized

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
