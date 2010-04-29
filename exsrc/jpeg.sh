#!/bin/sh
PACKAGE="jpeg"
. ./prolog.sh
CDMSARCH=`uname -m`
if (test "${CDMSARCH}" = "ia64") then
  export CC="gcc -fPIC"
fi
if (test "${CDMSARCH}" = "x86_64") then
  export CC="gcc -fPIC"
fi
(mkdir ${prefix}/Externals/HDF ; mkdir ${prefix}/Externals/HDF/lib ; mkdir ${prefix}/Externals/HDF/include ; cd jpeg* ; ./configure --prefix=${prefix}/Externals/HDF ; make ; mv libjpeg.a ${prefix}/Externals/HDF/lib ; cp *.h ${prefix}/Externals/HDF/include )

