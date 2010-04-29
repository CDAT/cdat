#!/bin/sh
PACKAGE="proj"
. ./prolog.sh
(cd proj* ; ./configure --prefix=${prefix}/Externals ; make ; make install )

