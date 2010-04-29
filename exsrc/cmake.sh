#!/bin/sh
PACKAGE="cmake"
. ./prolog.sh
(   cd cmake*; \
   ./configure --prefix=${prefix}/Externals; \
   make; make install
)
