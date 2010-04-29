#!/bin/sh
PACKAGE="Pyfort"
. ./prolog.sh
(cd Pyfort*; ${prefix}/${version}/bin/python setup.py build ${D} install )
