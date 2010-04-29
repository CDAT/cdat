#!/bin/sh
PACKAGE="fontconfig"
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

(cd fontconfig-* ; ./configure --prefix=${prefix}/Externals --enable-libxml2 --with-freetype-config=${prefix}/Externals/bin/freetype-config ; make ; make install )

