#!/bin/sh
PACKAGE="freetype"
. ./prolog.sh
CDMSARCH=`uname -m`
if (test "${CDMSARCH}" = "ia64") then
  export CC="gcc -fPIC"
fi
if (test "${CDMSARCH}" = "x86_64") then
  export CC="gcc -fPIC"
fi
(cd freetype-* ; ./configure --prefix=${prefix}/Externals ; make ; make install ; ln -s ${prefix}/Externals/include/freetype2/freetype ${prefix}/Externals/include/freetype )

