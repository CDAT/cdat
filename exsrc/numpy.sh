#!/bin/sh
PACKAGE="numpy"
. ./prolog.sh 
# Handle x86_64 arch
CDATARCH=`uname -m`
if (test "${CDATARCH}" = "x86_64") then
  cd numpy-*
  cat >site.cfg <<EOF
# Defaults
#  ========
# The settings given here will apply to all other sections if not overridden.
# This is a good place to add general library and include directories like
# /usr/local/{lib,include}
#
[DEFAULT]
library_dirs = /usr/lib64
EOF
  cd ..
fi
if (test "${BLAS}" = "SETBLAS") then
BLAS=${prefix}/Externals/lib/libblas.a
export BLAS
fi
if (test "${LAPACK}" = "SETLAPACK") then
LAPACK=${prefix}/Externals/lib/liblapack.a
export LAPACK
fi
# Numpy.
(cd numpy-*; ${prefix}/${version}/bin/python setup.py build ${D} install)

