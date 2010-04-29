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
# Try to find fox-1.7
#

FIND_PATH(FOX_INCLUDE_DIR fx.h
      /usr/include
      /usr/X11/include
      /usr/X11R6/include
      /sw/include
      ${VISUS_INCLUDE}/fox-1.7
)
FIND_LIBRARY(FOX_LIBRARIES FOX-1.7    
      /usr/lib
      /usr/X11/lib
      ${VISUS_LIBRARIES}
)

IF (FOX_INCLUDE_DIR AND FOX_LIBRARIES)
      SET(FOX_FOUND "Yes")
      MESSAGE("Using FOX_INCLUDE_DIR = " ${FOX_INCLUDE_DIR}) 
      MESSAGE("Using FOX_LIBRARIES   = " ${FOX_LIBRARIES}) 
ELSE (FOX_INCLUDE_DIR AND FOX_LIBRARIES)
  
   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("************************************")
      MESSAGE("  Necessary libfox-1.7 files not found")
      MESSAGE("  FOX_INCLUDE_DIR = " ${FOX_INCLUDE_DIR})
      MESSAGE("  FOX_LIBRARIES   = " ${FOX_LIBRARIES})
      MESSAGE("  libfox-1.7 will be build locally")
      MESSAGE("************************************")
   ENDIF (CMAKE_VERBOSE_MAKEFILE)
   
   IF (NOT EXISTS ${VISUS_EXTLIBS}/fox-1.7.9)    
      EXECUTE_PROCESS(                                        
           COMMAND gzip -cd ${VISUS_SOURCE_DIR}/ext-libs/fox-1.7.9.tar.gz 
           COMMAND tar xv
           WORKING_DIRECTORY ${VISUS_EXTLIBS}     
      )
   ENDIF (NOT EXISTS ${VISUS_EXTLIBS}/fox-1.7.9)      
   
   EXECUTE_PROCESS(                                        
      COMMAND ./configure --prefix=${VISUS_BINARY_DIR} #--enable-threadsafe 
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/fox-1.7.9 
   )
        
   EXECUTE_PROCESS(                                        
      COMMAND make install
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/fox-1.7.9
   )

   FIND_PATH(FOX_INCLUDE_DIR fx.h
      ${VISUS_INCLUDE}/fox-1.7
      NO_DEFAULT_PATH
   )

   FIND_LIBRARY(FOX_LIBRARIES FOX-1.7 
       ${VISUS_LIBRARIES}
       NO_DEFAULT_PATH
   )

   IF (FOX_INCLUDE_DIR AND FOX_LIBRARIES)
      SET(FOX_FOUND "YES")
      IF (CMAKE_VERBOSE_MAKEFILE)
         MESSAGE("Using FOX_INCLUDE_DIR = " ${FOX_INCLUDE_DIR}) 
         MESSAGE("Using FOX_LIBRARIES   = " ${FOX_LIBRARIES}) 
      ENDIF (CMAKE_VERBOSE_MAKEFILE)
   ELSE (FOX_INCLUDE_DIR AND FOX_LIBRARIES)
      MESSAGE("ERROR fox-1.7 not found on the system and could not be build")
   ENDIF (FOX_INCLUDE_DIR AND FOX_LIBRARIES)

ENDIF (FOX_INCLUDE_DIR AND FOX_LIBRARIES)
