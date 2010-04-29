#!/bin/sh
PACKAGE="lapack-lite"
BUILD=`pwd`
export BUILD
. ./prolog.sh

FC=`${prefix}/${version}/bin/python ${BUILD}/detect_fortran.py`
export FC
if ( test $FC = "gfortran") then
    CPPFLAGS="-DpgiFortran"; export CPPFLAGS
fi
if (test `uname` = "HP-UX") then
    CPPFLAGS="+z -D_HPUX_SOURCE"; export CPPFLAGS
elif (test `uname` = "Darwin") then
(    CXX=""; export CXX \
)
fi

cd lapack-lite*;\
    # Add f77 support
unset PGI; \
    echo $FC ; \
    env LOADER=${FC} FORTRAN=${FC} BLAS=${prefix}/Externals/libblas.a make; cp liblapack.a libtmglib.a ${prefix}/Externals/lib; \
    

