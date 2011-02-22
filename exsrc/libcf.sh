#!/bin/sh

PACKAGE="libcf"
BUILD=`pwd`
export BUILD
. ./prolog.sh

NC4LOC=`grep NC4LOC ../config.log | sed 's/NC4LOC=//' | sed "s/'//"`
HDF5LOC=`grep HDF5LOC ../config.log | sed 's/HDF5LOC=//' | sed "s/'//"`

echo "prefix is ${prefix}"
echo "using netcdf at $NC4LOC, using hdf5 at $HDF5LOC"

(cd libcf*; \ 
  mkdir ${prefix}/Externals/libcf ; \  
  mkdir ${prefix}/Externals/NetCDF ; \
  ./configure --prefix=${prefix}/Externals/NetCDF --with-netcdf=$NC4LOC --with-hdf5=$HDF5LOC --enable-shared; \
  make; make install
)

