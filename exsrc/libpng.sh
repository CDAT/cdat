#!/bin/sh
PACKAGE="libpng"
. ./prolog.sh
CDMSARCH=`uname -m`
if (test "${CDMSARCH}" = "ia64") then
  export CC="gcc -fPIC"
fi
if (test "${CDMSARCH}" = "x86_64") then
  export CC="gcc -fPIC"
fi
(cd libpng* ; ./configure --prefix=${prefix}/Externals ; make ; make install )

