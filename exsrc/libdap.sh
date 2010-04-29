#!/bin/sh
PACKAGE="libdap"
. ./prolog.sh
CDMSARCH=`uname -m`
if (test "${CDMSARCH}" = "ia64") then
  export CC="gcc -fPIC"
  export CXX="g++ -fPIC"
fi
if (test "${CDMSARCH}" = "x86_64") then
  export CC="gcc -fPIC"
  export CXX="g++ -fPIC"
fi
(cd libdap* ; env PATH=${prefix}/Externals/OpenDAP/bin\:${PATH} ./configure --disable-shared --prefix=${prefix}/Externals/OpenDAP ; make ; make install )

