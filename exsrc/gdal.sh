#!/bin/sh
PACKAGE="gdal"
. ./prolog.sh
(cd gdal* ; ./configure --with-libtiff=internal --with-gif=internal --without-cfitsio --prefix=${prefix}/Externals ; make ; make install; ${prefix}/${version}/bin/python setup.py install )

