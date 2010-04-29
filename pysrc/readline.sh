#!/bin/sh
. ./prolog.sh
cd readline-*
./configure --prefix=${prefix}/Externals
if (test $? -ne 0) then
    echo "readline configuration failed.";
    echo "Some platforms don't support readline, this doesn't matter.";
    echo "Ignoring this error.";
    exit 0;
fi
make
if (test $? -ne 0) then
    echo "readline make failed.";
    echo "Some platforms don't support readline, this doesn't matter.";
    echo "Ignoring this error.";
    exit 0;
fi
make install
if (test $? -ne 0) then
    echo "readline install failed.";
    echo "This is unexpected since it built ok.";
    exit 1;
fi
