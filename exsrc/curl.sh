#!/bin/sh
PACKAGE="curl"
. ./prolog.sh
CDMSARCH=`uname -m`
if (test "${CDMSARCH}" = "ia64") then
  export CC="gcc -fPIC"
fi
if (test "${CDMSARCH}" = "x86_64") then
  export CC="gcc -fPIC"
fi
(cd curl* ; ./configure --disable-shared --prefix=${prefix}/Externals/OpenDAP ; make ; make install )

