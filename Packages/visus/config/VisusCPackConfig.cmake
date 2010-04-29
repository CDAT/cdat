########################################################################
#
# Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
# Produced at the Lawrence Livermore National Laboratory  
# Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
# LLNL-CODE-406031.  
# All rights reserved.  
#   
# This file is part of "Simple and Flexible Scene Graph Version 2.0."
# Please also read BSD_ADDITIONAL.txt.
#   
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#   
# @ Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the disclaimer below.
# @ Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the disclaimer (as noted below) in
#   the documentation and/or other materials provided with the
#   distribution.
# @ Neither the name of the LLNS/LLNL nor the names of its contributors
#   may be used to endorse or promote products derived from this software
#   without specific prior written permission.
#   
#  
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
# LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING
#
########################################################################



SET (CPACK_SOURCE_IGNORE_FILES
        /CVS
        /.svn
        ~$
        .\#
        \#
        .DS_Store
        .project
        ./LLNL-bin
        ./Makefile
        README
        xcodeproj$
        ./TODO
        ./bin/
        ./doc/
        ./include/
        ./lib/
        ./man/
        ./share/
        ./rootFiles.txt
        ./python/
        ./start
        ./testing/
        ./utils/
        ./visualc/
        ./build/
        ./config/AIX
        ./config/aix
        ./config/bgl
        ./config/chaos
        ./config/common
        ./config/Darwin
        ./config/linux
        ./config/mf
        ./config/mklinux
        ./config/sphere
        ./config/SunOS
        ./config/thunder
        ./config/tux
        ./config/Valerio
        ./config/gcc-LC
        ./config/linux
        ./config/irix6.5
)

IF (NOT VISUS_ENABLE_LEGACY)
   SET (CPACK_SOURCE_IGNORE_FILES
        ${CPACK_SOURCE_IGNORE_FILES}
        ./src/fortran
        ./src/miranda
        ./src/pv
        ./src/scenegraph
        ./src/napa
        ./src/libvisus
        ./src/piv
        ./src/impact
        ./src/prototype
        ./src/sandbox
        ./src/matlab
        ./src/liblocal
        ./src/util
        ./src/silo
        ./src/volrend
        ./src/volrend-old

        ./ext-libs/expect-5.43.tar.gz
        ./ext-libs/fltk-1.1.6.tar.gz
        ./ext-libs/fox-1.4.10.tar.gz
        ./ext-libs/fox-1.7.9.tar.gz
        ./ext-libs/jpegsrc.v6b.tar.gz
        ./ext-libs/libxml2-2.5.9.tar.gz
        ./ext-libs/miranda-visus-1.1-no-visus.tar.gz
        ./ext-libs/netcdf-3.5.1.tar.gz
        ./ext-libs/netcdf-3.6.0.tar.gz
        ./ext-libs/silo011105.tar.gz
        ./ext-libs/tcl8.4.9-src.tar.gz
        ./ext-libs/liblocal.tar.gz

        
   )
ENDIF (NOT VISUS_ENABLE_LEGACY)

IF (NOT VISUS_ENABLE_IDX)
   SET (CPACK_SOURCE_IGNORE_FILES
        ${CPACK_SOURCE_IGNORE_FILES}
        ./src/visus3d
   )
ENDIF (NOT VISUS_ENABLE_IDX)
        
IF (NOT VISUS_ENABLE_FTGL)
   SET (CPACK_SOURCE_IGNORE_FILES
        ${CPACK_SOURCE_IGNORE_FILES}
        ./ext-libs/ftgl-2.1.2.tar.gz
   )
ENDIF (NOT VISUS_ENABLE_FTGL)
        
IF (NOT VISUS_ENABLE_FLTK)
   SET (CPACK_SOURCE_IGNORE_FILES
        ${CPACK_SOURCE_IGNORE_FILES}
        ./ext-libs/fltk-2.0.tar.gz
   )
ENDIF (NOT VISUS_ENABLE_FLTK)
        

        

SET (CPACK_PACKAGE_NAME "Visus")
SET (CPACK_PACKAGE_VERSION_MAJOR "2")
SET (CPACK_PACKAGE_VERSION_MINOR "0")
SET (CPACK_PACKAGE_VERSION_PATCH "0")
SET (CPACK_PACKAGE_INSTALL_DIRECTORY "Visus-${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH}")
SET (CPACK_PACKAGE_VENDOR "Lawrence Livermore National Laboratory")
SET (CPACK_PACKAGING_INSTALL_PREFIX ./)
SET (CMAKE_INSTALL_PREFIX ${VISUS_BINARY_DIR})
SET (CPACK_GENERATOR "TGZ")
SET (CPACK_SOURCE_GENERATOR "TGZ")
