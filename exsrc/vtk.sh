#!/bin/sh
PACKAGE="VTK"
. ./prolog.sh
(  BUILD_DIR=`pwd`;\
   cd VTK*; \
   sed -e 's@CDAT_PREFIX@'${prefix}'/Externals@g' \
       -e 's/PY_VERSION/2.4/g' \
       -e 's@CDAT_BUILD_DIR@'${BUILD_DIR}'@g' \
       -e 's/TCLTK_VERSION/8.4/g' ../../VTK_BUILD_ANSWERS.core > VTK_BUILD_ANSWERS.feed ; \
   mkdir -p ${prefix}/Externals/VTK;\
   cp VTK_BUILD_ANSWERS.feed ${prefix}/Externals/VTK/CMakeCache.txt ;
   cd ${prefix}/Externals/VTK ;\
   ${prefix}/Externals/bin/cmake CMakeCache.txt ;\
   make; make install ; \
   cd Wrapping/Python ; \
   ${prefix}/${version}/bin/python setup.py install; \
)
