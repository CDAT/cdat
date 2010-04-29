#!/bin/sh
PACKAGE="Twisted"
. ./prolog.sh 
# Twisted.
(cd Twisted-*/zope.interface*; ${prefix}/${version}/bin/python setup.py build ${D} install; cd .. ; ${prefix}/${version}/bin/python setup.py build ${D} install)

