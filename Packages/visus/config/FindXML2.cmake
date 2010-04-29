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
# Try to find xml2. 
#

SET (LibXml2_FIND_QUIETLY "YES")

INCLUDE(${CMAKE_ROOT}/Modules/FindLibXml2.cmake)
		

#MESSAGE("Using LIBXML2_LIBRARIES = " ${LIBXML2_LIBRARIES})     

IF (NOT LIBXML2_FOUND)
   FIND_PATH(LIBXML2_INCLUDE_DIR libxml/parser.h
       /usr/include
       /usr/include/libxml2
       /sw/include
       /sw/include/libxml2
       ${VISUS_INCLUDE}
       ${VISUS_INCLUDE}/libxml2
   )

   FIND_LIBRARY(LIBXML2_LIBRARIES xml2 
       /usr/lib
       /sw/lib
       ${VISUS_LIBRARIES}
   )
   IF (LIBXML2_LIBRARIES AND LIBXML2_INCLUDE_DIR)
      SET(LIBXML2_FOUND "YES")
   ENDIF (LIBXML2_LIBRARIES AND LIBXML2_INCLUDE_DIR)
ELSEIF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("Using LIBXML2_INCLUDE_DIR = " ${LIBXML2_INCLUDE_DIR})     
      MESSAGE("Using LIBXML2_LIBRARIES = " ${LIBXML2_LIBRARIES})     
ENDIF (NOT LIBXML2_FOUND)

IF (NOT LIBXML2_FOUND)
   
   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("************************************")
      MESSAGE("  Necessary xml2 files not found")
      MESSAGE("  libxml2 will be build locally")
      MESSAGE("************************************")
   ENDIF (CMAKE_VERBOSE_MAKEFILE)
   
   IF (NOT EXISTS ${VISUS_EXTLIBS}/libxml2-2.5.9)    
      EXECUTE_PROCESS(                                        
           COMMAND gzip -cd ${VISUS_SOURCE_DIR}/ext-libs/libxml2-2.5.9.tar.gz 
           COMMAND tar xv
           WORKING_DIRECTORY ${VISUS_EXTLIBS}     
      )
   ENDIF (NOT EXISTS ${VISUS_EXTLIBS}/libxml2-2.5.9) 
   
   EXECUTE_PROCESS(                                        
      COMMAND ./configure --prefix=${VISUS_EXT_PREFIX} 
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/libxml2-2.5.9    
   )
        
   EXECUTE_PROCESS(                                        
      COMMAND make install
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/libxml2-2.5.9     
   )

   FIND_PATH(LIBXML2_INCLUDE_DIR libxml/parser.h
       ${VISUS_EXT_PREFIX}/include
       ${VISUS_EXT_PREFIX}/include/libxml2
       NO_DEFAULT_PATH
   )

   FIND_LIBRARY(LIBXML2_LIBRARIES NAMES xml2 xml2.2 xml2.2.5
       ${VISUS_EXT_PREFIX}/lib
       NO_DEFAULT_PATH
   )

   IF (LIBXML2_LIBRARIES AND LIBXML2_INCLUDE_DIR)
      SET(LIBXML2_FOUND "YES")
      IF (CMAKE_VERBOSE_MAKEFILE)
         MESSAGE("Using LIBXML2_LIBRARIES = " ${LIBXML2_LIBRARIES}) 
      ENDIF (CMAKE_VERBOSE_MAKEFILE)
   ELSE (LIBXML2_LIBRARIES AND LIBXML2_INCLUDE_DIR)
      MESSAGE("ERROR libxml2 not found on the system and could not be build")
   ENDIF (LIBXML2_LIBRARIES AND LIBXML2_INCLUDE_DIR)

ENDIF (NOT LIBXML2_FOUND)

