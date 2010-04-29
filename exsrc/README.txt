This directory cannot be built until Python is built.

This directory contains sources for some parts of the CDAT
system that we didn't write or which change on very slow timescales. 

./install_script /whereyouwanttoputit 

The subdirectory src contains the tarred/zipped files that are used to make
the product. A subdirectory build will be created that contains the output.
Some of these products can be tested by changing to their directory under 
build and typing "make test".

This process will unpack the tar files from the src directory if there is no 
build subdirectory. Otherwise it doesn't. If you put in a new source file
into src you need to clean before building.

Log files are created in the build subdirectory.

Each of the pieces may be built individually using the corresponding .sh 
files in this directory. Some warning errors are usual from 
many of the packages and vary from architecture to architecture.


