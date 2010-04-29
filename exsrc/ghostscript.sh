#!/bin/sh
PACKAGE="ghostscript"
. ./prolog.sh
(mkdir -p ${prefix}/Externals/share/ghostscript ; cd ghostscript-*; ln -s ../libpng-1.2.8 libpng ; ln -s ../jpeg-6b jpeg ; ./configure --prefix=${prefix}/Externals ; make ; make install ; mv ../fonts ${prefix}/Externals/share/ghostscript )

