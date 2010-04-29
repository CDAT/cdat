#!/bin/sh
PACKAGE="ipython1"
. ./prolog.sh 
# ipython1.
(cd ipython1*; ${prefix}/${version}/bin/python setup.py build ${D} install)

