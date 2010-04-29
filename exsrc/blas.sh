#!/bin/sh
PACKAGE="blas"
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

cd blas*;\
    # Add f77 support
unset PGI; \
    echo $FC ; \
    env FORTRAN=${FC} make; cp libblas.a ${prefix}/Externals/lib; \
    

