#!/bin/sh
PACKAGE=gplot
. ./prolog.sh
d=`uname`
(cd gplot; make -f Makefile.${d} ; mv gplot ${prefix}/Externals/bin )

