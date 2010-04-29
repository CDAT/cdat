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


# 
# Try to find zlib.
#


# First we try to find zlib in the system
INCLUDE(${CMAKE_ROOT}/Modules/FindZLIB.cmake)


# If we found a libz installed we must check whether it has 
# the functionality we need

INCLUDE(CheckLibraryExists)

IF (ZLIB_FOUND)
   CHECK_LIBRARY_EXISTS(${ZLIB_LIBRARIES} compressBound "" ZLIB_COMPATIBLE)

   # If the system on is not acceptable we haven't found what we were
   # looking for
   IF (NOT ZLIB_COMPATIBLE)
      SET(ZLIB_FOUND "NO")
      SET(ZLIB_INCLUDE_DIR ZLIB_INCLUDE_DIR-NOTFOUND)
      SET(ZLIB_LIBRARIES ZLIB_LIBRARIES-NOTFOUND)
   ENDIF (NOT ZLIB_COMPATIBLE)
ENDIF (ZLIB_FOUND)
        
# If we could not find a compatible system libz we try to see whether
# we have installed our own libz already

IF (NOT ZLIB_FOUND)
   FIND_PATH(ZLIB_INCLUDE_DIR zlib.h
       ${VISUS_INCLUDE}
       NO_DEFAULT_PATH
   )

   FIND_LIBRARY(ZLIB_LIBRARIES z
       ${VISUS_LIBRARIES}
       NO_DEFAULT_PATH
   )
   
      MESSAGE("Using ZLIB_INCLUDE_DIR = " ${ZLIB_INCLUDE_DIR})
      MESSAGE("Using ZLIB_LIBRARIES = " ${ZLIB_LIBRARIES})     

   IF (ZLIB_LIBRARIES AND ZLIB_INCLUDE_DIR)
      SET(ZLIB_FOUND "YES")
   ENDIF (ZLIB_LIBRARIES AND ZLIB_INCLUDE_DIR)
ELSEIF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("Using ZLIB_INCLUDE_DIR = " ${ZLIB_INCLUDE_DIR})
      MESSAGE("Using ZLIB_LIBRARIES = " ${ZLIB_LIBRARIES})     
ENDIF (NOT ZLIB_FOUND)

# If we have not found libz anywhere we try to build it

IF (NOT ZLIB_FOUND)
   
   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("************************************")
      MESSAGE("  Necessary zlib files not found")
      MESSAGE("  libz will be build locally")
      MESSAGE("************************************")
   ENDIF (CMAKE_VERBOSE_MAKEFILE)
   
   IF (NOT EXISTS ${VISUS_EXTLIBS}/zlib-1.2.3)    
      EXECUTE_PROCESS(                                        
           COMMAND gzip -cd ${VISUS_SOURCE_DIR}/ext-libs/zlib-1.2.3.tar.gz 
           COMMAND tar xv
           WORKING_DIRECTORY ${VISUS_EXTLIBS}     
      )
   ENDIF (NOT EXISTS ${VISUS_EXTLIBS}/zlib-1.2.3) 
   
   EXECUTE_PROCESS(                                        
      COMMAND ./configure --shared --prefix=${VISUS_EXT_PREFIX} 
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/zlib-1.2.3     
   )
        
   EXECUTE_PROCESS(                                        
      COMMAND make install
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/zlib-1.2.3     
   )

   # Once installed make sure the libraries are where they are
   # supposed to be
   FIND_LIBRARY(ZLIB_LIBRARIES z
       ${VISUS_EXT_PREFIX}/lib 
       NO_DEFAULT_PATH
   )

   FIND_PATH(ZLIB_INCLUDE_DIR zlib.h
       ${VISUS_EXT_PREFIX}/include
       NO_DEFAULT_PATH
   )

   IF (NOT ZLIB_LIBRARIES)
      MESSAGE("ERROR zlib not found on the system and could not be build")
   ELSE (NOT ZLIB_LIBRARIES)
      SET(ZLIB_FOUND "YES")
      IF (CMAKE_VERBOSE_MAKEFILE)
         MESSAGE("Using ZLIB_INCLUDE_DIR = " ${ZLIB_INCLUDE_DIR})
         MESSAGE("Using ZLIB_LIBRARIES = " ${ZLIB_LIBRARIES}) 
      ENDIF (CMAKE_VERBOSE_MAKEFILE)
   ENDIF (NOT ZLIB_LIBRARIES)                           
ENDIF (NOT ZLIB_FOUND)

# Under windows we want to prevent the internal ZLIB_LIBRARY variable
# from appearing as unset since we only use ZLIB_LIBRARIES

IF (WIN32) 
   IF (NOT ZLIB_FOUND)
      SET(ZLIB_LIBARY "Yes")
   ENDIF (NOT ZLIB_FOUND)
ENDIF (WIN32) 
