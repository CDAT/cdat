#!/bin/sh
. ./prolog.sh
sitepackages=`${prefix}/bin/python ../find_site.py`
(cd Fnorb-*; \
fullpath=`pwd`;
FNORB=`basename ${fullpath}`; \
cd src; \
make -f Makefile.pre.in boot; \
make; \
/bin/cp *.so ${sitepackages}; \
cd ../..; \
if (test -d ${sitepackages}/${FNORB}) then \
   chmod -R +w ${sitepackages}/${FNORB}; \
fi; \
/bin/cp -R ${FNORB} ${sitepackages}; \
if (test ! -d ${sitepackages}/Fnorb) then \
   ln -s ${sitepackages}/${FNORB} ${sitepackages}/Fnorb; \
fi; )

