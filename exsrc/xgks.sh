#!/bin/sh
PACKAGE="xgks"
OS=`uname`
. ./prolog.sh
# xgks
if ( test "${OS}" = 'Darwin' ) then
    CPP_X11="-I/usr/X11R6/include"; export CPP_X11
fi
LD_X11=""; export LD_X11
FC='';export FC
# The configure step will make a header file udposix.h that vcs needs
cd xgks 
./configure --prefix=${prefix}/Externals || exit 1
echo "Installing udposix.h"
/bin/rm -fr ${prefix}/Externals/include/udposix.h || exit 1
/bin/cp port/misc/udposix.h ${prefix}/Externals/include/udposix.h || exit 1
make port/all || exit 1
make port/install || exit 1
# added the CXX define for MacOS
make CXX=cc fontdb/all || exit 1
make CXX=cc fontdb/install || exit 1
