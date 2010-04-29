#!/bin/sh
. ./prolog.sh
# tcl
cd tcl8*
cd unix
if (test "${OS}" = "Darwin") then  # MacIntosh OSX
   ./configure --prefix=${prefix}/Externals
else
   ./configure --disable-shared --prefix=${prefix}/Externals
fi

if (test $? -ne 0) then
    echo "tcl configuration failed.";
    exit 1;
fi
make
if (test $? -ne 0) then
    echo "tcl make failed.";
    exit 1;
fi
make install
if (test $? -ne 0) then
    echo "tcl install failed.";
    exit 1;
fi
