#!/bin/sh
PACKAGE="R"
. ./prolog.sh
(cd R*; ./configure --enable-R-shlib --prefix=${prefix}/Externals/R ; make ; make install ; make install ; cd ${prefix}/Externals/R/lib ; ln -s Externals/R/bin/libR.so )

