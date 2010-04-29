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
#
# Try to find the FTGL libraries
# Once done this will define
#
# FTGL_FOUND          - system has ftgl
# FTGL_INCLUDE_DIR    - path to FTGL/FTGL.h
# FTGL_LIBRARIES      - the library that must be included
#
#


FIND_PATH(FTGL_INCLUDE_DIR FTGL/FTGL.h
       ${ADDITIONAL_INCLUDE_PATH}
      /usr/include
      /usr/X11/include
      /usr/X11R6/include
      ${VISUS_INCLUDE}  
)

IF (NOT WIN32)

FIND_FILE(FTGL_LIBRARIES libftgl.a  
       ${ADDITIONAL_LIBRARY_PATH}
      /usr/lib
      /usr/X11/lib
      /usr/X11R6/lib
      /sw/lib
      ${VISUS_LIBRARIES}  
)

ELSE (NOT WIN32)

FIND_LIBRARY(FTGL_LIBRARIES NAMES ftgl_static_MT ftgl_static
      PATHS 
      ${ADDITIONAL_LIBRARY_PATH}
      /usr/lib
      /usr/X11/lib
      /usr/X11R6/lib
      /sw/lib
      ${VISUS_LIBRARIES}  
)

ENDIF (NOT WIN32)


IF (FTGL_INCLUDE_DIR AND FTGL_LIBRARIES)

   SET(FTGL_FOUND "YES")
   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("Using FTGL_INCLUDE_DIR = " ${FTGL_INCLUDE_DIR}) 
      MESSAGE("Using FTGL_LIBRARIES   = " ${FTGL_LIBRARIES}) 
   ENDIF (CMAKE_VERBOSE_MAKEFILE)

ELSE (FTGL_INCLUDE_DIR AND FTGL_LIBRARIES)
   
   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("************************************")
      MESSAGE("  Necessary libftgl files not found")
      MESSAGE("  FTGL_INCLUDE_DIR = " ${FTGL_INCLUDE_DIR})
      MESSAGE("  FTGL_LIBRARIES   = " ${FTGL_LIBRARIES})
      MESSAGE("  libftgl will be build locally")
      MESSAGE("************************************")
   ENDIF (CMAKE_VERBOSE_MAKEFILE)
   
   IF (APPLE_NATIVE_GL)
      INCLUDE(CheckCXXSourceCompiles)
      SET(CMAKE_REQUIRED_INCLUDES ${OPENGL_INCLUDE_DIR})
      SET(CMAKE_REQUIRED_LIBRARIES ${OPENGL_gl_LIBRARY} ${OPENGL_glu_LIBRARY})
      CHECK_CXX_SOURCE_COMPILES( "
           #include <OpenGL/glu.h>
           typedef GLvoid (*TF)(); 
           int main() { 
           gluTessCallback(0,GLU_TESS_END_DATA,(TF)0);
           return 1;
           }" FTGL_GLU_EMPTY)
      CHECK_CXX_SOURCE_COMPILES( "
           #include <OpenGL/glu.h>
           typedef GLvoid (*TF)(...); 
           int main() { 
           gluTessCallback(0,GLU_TESS_END_DATA,(TF)0);return 1;
           }" FTGL_GLU_DOTS)
   
      IF (CMAKE_VERBOSE_MAKEFILE AND NOT FTGL_GLU_EMPTY AND NOT FTGL_GLU_DOTS)
           MESSAGE("************************************")
           MESSAGE(" Could not compile glu test code    ")
           MESSAGE(" using default behavior    ")
           MESSAGE(" FTGL_GLU_EMPTY " ${FTGL_GLU_EMPTY})
           MESSAGE(" FTGL_GLU_DOTS " ${FTGL_GLU_DOTS})
           MESSAGE("************************************")
      ENDIF (CMAKE_VERBOSE_MAKEFILE AND NOT FTGL_GLU_EMPTY AND NOT FTGL_GLU_DOTS)
      
      IF (FTGL_GLU_EMPTY)
         SET(FTLG_GLU_FLAGS "CXXFLAGS=-DFTGL_GLU_EMPTY=1")
      ELSEIF (FTGL_GLU_DOTS)
         SET(FTLG_GLU_FLAGS "CXXFLAGS=-DFTGL_GLU_DOTS=1")
      ENDIF (FTGL_GLU_EMPTY)
   ENDIF (APPLE_NATIVE_GL)
            

   IF (NOT EXISTS ${VISUS_EXTLIBS}/FTGL)    
      EXECUTE_PROCESS(                                        
	    COMMAND gzip -cd ${VISUS_SOURCE_DIR}/ext-libs/ftgl-2.1.2.tar.gz 
	    COMMAND tar xv
	    WORKING_DIRECTORY ${VISUS_EXTLIBS}
      )	
   ENDIF (NOT EXISTS ${VISUS_EXTLIBS}/FTGL)      
   
   # 
   # On some Macs the -with-gl-lib=xxx/OpenGL.framework flag does not work 
   # as a quick hack around this issue we append the standard name of the 
   # necessary library
   #
   IF (APPLE_NATIVE_GL)
      SET(OPENGL_gl_LIBRARY ${OPENGL_gl_LIBRARY}/Libraries)
   ENDIF (APPLE_NATIVE_GL)

   EXECUTE_PROCESS(                                    
      COMMAND ./configure ${FTLG_GLU_FLAGS} --prefix=${VISUS_EXT_PREFIX} --with-freetype-prefix=${FREETYPE2_FT_CONFIG} --with-gl-inc=${OPENGL_INCLUDE_DIR} --with-gl-lib=${OPENGL_gl_LIBRARY} --enable-shared=yes
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/FTGL/unix  
   )

   EXECUTE_PROCESS(                                        
      COMMAND make install
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/FTGL/unix    
   )

   FIND_PATH(FTGL_INCLUDE_DIR FTGL/FTGL.h
       ${VISUS_EXT_PREFIX}/include
       NO_DEFAULT_PATH
   )

   FIND_FILE(FTGL_LIBRARIES libftgl.a
       ${VISUS_EXT_PREFIX}/lib
       NO_DEFAULT_PATH
   )

   IF (FTGL_LIBRARIES AND  FTGL_INCLUDE_DIR)
      SET(FTGL_FOUND "YES")
      IF (CMAKE_VERBOSE_MAKEFILE)
         MESSAGE("Using FTGL_INCLUDE_DIR = " ${FTGL_INCLUDE_DIR}) 
         MESSAGE("Using FTGL_LIBRARIES   = " ${FTGL_LIBRARIES}) 
      ENDIF (CMAKE_VERBOSE_MAKEFILE)
 
   ELSE (FTGL_LIBRARIES AND  FTGL_INCLUDE_DIR)
      MESSAGE("ERROR ftgl library not found on the system and could not be build")
   ENDIF (FTGL_LIBRARIES AND  FTGL_INCLUDE_DIR)

ENDIF (FTGL_INCLUDE_DIR AND FTGL_LIBRARIES)
                         
