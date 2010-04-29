#!/bin/sh
PACKAGE="HDF"
. ./prolog.sh
CDMSARCH=`uname -m`
if (test "${CDMSARCH}" = "ia64") then
  export CC="gcc -fPIC"
fi
if (test "${CDMSARCH}" = "x86_64") then
  export CC="gcc -fPIC"
fi
(cd HDF* ; env CFLAGS=-DHAVE_NETCDF CXXFLAGS=-DHAVE_NETCDF ./configure --enable-fortran=no --disable-shared --with-jpeg=${prefix}/External/HDF --prefix=${prefix}/Externals/HDF ; make ; make install ; cp -pf ${prefix}/Externals/HDF/bin/* ${prefix}/Externals/bin )

