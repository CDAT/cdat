#!/bin/sh
PACKAGE="netpbm"
OS=`uname`
if ( test "${OS}" = 'Darwin' ) then
    echo "Darwin" ;
    CONF_FILE=netpbm.input.conf.Darwin;
elif ( test "${OS}" = 'sunOS' ) then
    echo "Sun OS";
    CONF_FILE=netpbm.input.conf.sun;
elif ( test "${OS}" = 'Linux' ) then
    echo "GNU Linux";
    CONF_FILE=netpbm.input.conf;
elif ( test "${OS}" = 'CYGWIN_NT-5.1' ) then
    echo "GNU Build for Cygwin";
    CONF_FILE=netpbm.input.conf.Cygwin;
elif ( test "${OS}" = 'CYGWIN_NT-6.0' ) then
    echo "GNU Build for Cygwin";
    CONF_FILE=netpbm.input.conf.Cygwin;
else
    echo "Platform not tested, using GNU conf file";
    echo "If hangs or fails try manually or use pbmplus";
fi
. ./prolog.sh
(  
   cd netpbm*; \
   BUILD_DIR=`pwd`;\
   sed -e 's@CDAT_PREFIX@'${prefix}'/Externals@g' \
       -e 's@INST_PREFIX@'${BUILD_DIR}'/TMP@g' \
        ../../netpbm.input.inst > netpbm.input.inst.feed ; \
   ./configure < ../../${CONF_FILE} ; \
   make ; \
   make package pkgdir=${BUILD_DIR}/TMP; \
   ./installnetpbm < netpbm.input.inst.feed ; \
   rm -rf  ${BUILD_DIR}/TMP 
)
