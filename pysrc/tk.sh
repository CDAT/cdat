#!/bin/sh
. ./prolog.sh
cd tk8*
cd unix
if (test "${OS}" = "Darwin") then  # MacIntosh OSX
   ./configure --prefix=${prefix}/Externals
else
   ./configure --disable-shared --prefix=${prefix}/Externals
fi

if (test $? -ne 0) then
    echo "tk configuration failed.";
    exit 1;
fi
make
if (test $? -ne 0) then
    echo "tk make failed.";
    exit 1;
fi
make install
if (test $? -ne 0) then
    echo "tk installation failed.";
    exit 1;
fi
