#!/bin/sh
PACKAGE="Pmw"
. ./prolog.sh 
# Twisted.
(cd Pmw-* ; cd src;  ${prefix}/${version}/bin/python setup.py build ${D} install)

