#!/bin/sh
. ./prolog.sh $*
cd zlib-*
CDMSARCH=`uname -m`
if (test "${CDMSARCH}" = "ia64") then
  export CC="gcc -fPIC"
fi
if (test "${CDMSARCH}" = "x86_64") then
  export CC="gcc -fPIC"
fi
./configure --prefix=${prefix}/Externals
if (test $? -ne 0) then
    echo "zlib configuration failed.";
    exit 1;
fi
make
if (test $? -ne 0) then
    echo "zlib make failed.";
    exit 1;
fi
make install
if (test $? -ne 0) then
    echo "zlib installation failed.";
    exit 1;
fi
