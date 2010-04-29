This directory contains all the sources for building a Python suitable for
use with CDAT. 

Changes from standard distributions:
   a. readline
      In file readline.c, change definition of RL_LIBRARY_VERSION to avoid
      the error if this macro is already defined, by undefining it.
   b. We use a private version of Python's setup.py to have it find
      our own tcl/tk.

To install:
./install_script /whereyouwanttoputit 

A subdirectory build will be created that contains the output.
Some of these products can be tested by changing to their directory under 
build and typing "make test".

If you put in a new source file you need to remove the old one and run
./clean_script before building again.


OPTIONS:
you can add: --enable-aqua to the build line to prevent the build of Tcl/Tk
and use Aqua Native
you can add: --disable-tkbuild to the build line to prevent the build of Tcl/Tk

Log files are created in the build subdirectory.

Each of the pieces may be built individually using the corresponding .sh 
files in this directory. Some warning errors are usual from 
many of the packages and vary from architecture to architecture.

N.B.: The order in which the packages are built matters. 

You can add an 'exit 0' at any appropriate point in install_script if you
want to go up to that point and then stop.
