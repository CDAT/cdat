#!/bin/sh
PACKAGE="netcdf"
. ./prolog.sh
if (test `uname` = "HP-UX") then
    CPPFLAGS="+z -D_HPUX_SOURCE"; export CPPFLAGS
elif (test `uname` = "Darwin") then
    CXX=""; export CXX
fi

echo "prefix is"${prefix}
# Define compilation flags for itanium based NEC TX-7 (and gcc) -> ia64
# Also define compilation flags for SGI Altrix (and gcc) -> ia64
# Same for AMD Opteron based HP Proliant DL585                  -> x86_64
# export CFLAGS="$CFLAGS -fpic -O"
CDMSARCH=`uname -m`
if (test "${CDMSARCH}" = "ia64") then
    export CFLAGS="$CFLAGS -fPIC"
fi
if (test "${CDMSARCH}" = "x86_64") then
    export CFLAGS="$CFLAGS -fPIC"
fi

if (test `uname ` = "CYGWIN_NT-5.1") then
(cd netcdf*; \
    FC=''; export FC; \
    F90='';export F90; \
    unset PGI; \
    mkdir ${prefix}/Externals/NetCDF ; \
    ./configure --build=i686-pc-linux-gnu --prefix=${prefix}/Externals/NetCDF; \
    make; make install
)
elif (test `uname ` = "CYGWIN_NT-6.0") then
(cd netcdf*; \
    FC=''; export FC; \
    F90='';export F90; \
    unset PGI; \
    mkdir ${prefix}/Externals/NetCDF ; \
    ./configure --build=i686-pc-linux-gnu --prefix=${prefix}/Externals/NetCDF; \
    make; make install
)
else
(cd netcdf*; \
    FC=''; export FC; \
    F90='';export F90; \
    unset PGI; \
    mkdir ${prefix}/Externals/NetCDF ; \
    ./configure --prefix=${prefix}/Externals/NetCDF; \
    make; make install
)
fi
