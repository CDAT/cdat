#!/bin/sh
PACKAGE="ipython"
. ./prolog.sh 
# ipython.
(cd ipython-* ; ${prefix}/${version}/bin/python setup.py build ${D} install)
