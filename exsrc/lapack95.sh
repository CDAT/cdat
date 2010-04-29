#!/bin/sh
PACKAGE="lapack95"
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

cd lapack95*/SRC;\
    # Add f77 support
unset PGI; \
    echo $FC ; \
    env LAPACK_PATH=${prefix}/Externals/lib make; cp ../lapack95.a  ${prefix}/Externals/lib/liblapack95.a; cp ../lapack95_modules/* ${prefix}/Externals/include  \
    

