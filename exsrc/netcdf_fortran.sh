#!/bin/sh
PACKAGE="netcdf"
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

if (test `uname ` = "CYGWIN_NT-5.1") then 
(cd netcdf*; \
    unset PGI; \
    mkdir ${prefix}/Externals/NetCDF; \
    ./configure --build=i686-pc-linux-gnu --prefix=${prefix}/Externals/NetCDF; \
    make; make install
)
elif (test `uname ` = "CYGWIN_NT-6.0") then 
(cd netcdf*; \
    unset PGI; \
    mkdir ${prefix}/Externals/NetCDF; \
    ./configure --build=i686-pc-linux-gnu --prefix=${prefix}/Externals/NetCDF; \
    make; make install
)
else
(cd netcdf*;\
    # Add f77 support
    unset PGI; \
    mkdir ${prefix}/Externals/NetCDF; \
    ./configure --prefix=${prefix}/Externals/NetCDF; \
    make; make install; \
)
fi

