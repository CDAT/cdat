#!/bin/sh
PACKAGE="gifsicle"
. ./prolog.sh
(cd gifsicle*; ./configure --prefix=${prefix}/Externals ; make install )

