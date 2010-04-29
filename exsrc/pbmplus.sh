#!/bin/sh
PACKAGE="pbmplus"
. ./prolog.sh
d=`uname`
if ( test "Linux" = "${d}" ) then
( INSTALLBINARIES=${prefix}/Externals/bin; export INSTALLBINARIES; INSTALLMANUALS=${prefix}/Externals/man/mann ; export INSTALLMANUALS ; CC="gcc -ansi" ; export CC;cd pbmplus; make install )
else
( INSTALLBINARIES=${prefix}/Externals/bin; export INSTALLBINARIES ; INSTALLMANUALS=${prefix}/Externals/man/mann ; export INSTALLMANUALS ; cd pbmplus; make install )
fi
