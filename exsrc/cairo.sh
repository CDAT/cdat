#!/bin/sh
PACKAGE="cairo"
. ./prolog.sh
CDMSARCH=`uname -m`
if (test "${CDMSARCH}" = "ia64") then
  export CC="gcc -fPIC"
fi
if (test "${CDMSARCH}" = "x86_64") then
  export CC="gcc -fPIC"
fi
PKG_CONFIG=${prefix}/Externals/bin/pkg-config
export PKG_CONFIG
(cd cairo-* ; ./configure --prefix=${prefix}/Externals ; make ; make install )

