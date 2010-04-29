#!/bin/sh
BUILD=`pwd`
export BUILD
PACKAGE="ioapi"
. ./prolog.sh
FC=`${prefix}/${version}/bin/python ${BUILD}/detect_fortran.py`
export FC
(cd ioapi*/ioapi; \
    # build the library
    make -f Makefile.nocpl; \
    # go to the object/lib directory
    # and run ranlib (only needed for Darwin)
    # but doesn't effect the build
    cd ../neutral_g77; \
    ranlib libioapi.a; \

    # copy the library to pyIoapi contrib package
    # and the installation directory (prefix)
#    echo "Copying IOAPI library to pyIoapi package" ; \
#    cp libioapi.a ../../../../contrib/pyIoapi/Src/lib_external; \
    cp libioapi.a ${prefix}/Externals/lib;
)
