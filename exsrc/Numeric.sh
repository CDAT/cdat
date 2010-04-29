#!/bin/sh
PACKAGE="Numeric"
. ./prolog.sh 
CDMSARCH=`uname -m`
if (test "${CDMSARCH}" = "ia64") then
   echo "Numeric won't build on 64bit system, use numpy instead"
   exit
fi
if (test "${CDMSARCH}" = "x86_64") then
   echo "Numeric won't build on 64bit system, use numpy instead"
   exit
fi

# Numeric, MA, PropertiedClasses, etc.
(cd Numeric-*; ${prefix}/${version}/bin/python setup.py build ${D} install)

