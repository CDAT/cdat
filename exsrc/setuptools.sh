#!/bin/sh
PACKAGE="setuptools"
. ./prolog.sh 
# Twisted.
(cd setuptools-* ; ${prefix}/${version}/bin/python setup.py build ${D} install)

