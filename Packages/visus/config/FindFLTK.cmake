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
# Try to find libfltk2

#  Platform dependent libraries required by FLTK
IF(WIN32)
  IF(NOT CYGWIN)
    IF(BORLAND)
      SET( FLTK_PLATFORM_DEPENDENT_LIBS import32 )
    ELSE(BORLAND)
      SET( FLTK_PLATFORM_DEPENDENT_LIBS wsock32 comctl32 )
    ENDIF(BORLAND)
  ENDIF(NOT CYGWIN)
ENDIF(WIN32)

IF(UNIX)
  INCLUDE(${CMAKE_ROOT}/Modules/FindX11.cmake)

  # Newer X servers seem to need this library 
  # for fltk. Since I have no clue how to define 
  # this in the cmake file we just look for the 
  # library and use it if we find it
  FIND_LIBRARY(XINERAMA_LIBRARY Xinerama
     /usr/lib
     /usr/X11/lib
     /usr/X11R6/lib    
  )

  FIND_LIBRARY(XFT_LIBRARY Xft
     /usr/lib
     /usr/X11/lib
     /usr/X11R6/lib    
  )

  SET( FLTK2_PLATFORM_DEPENDENT_LIBS ${X11_LIBRARIES} -lm ${XFT_LIBRARY} ${XINERAMA_LIBRARY})
ENDIF(UNIX)

IF(APPLE)
  SET( FLTK2_PLATFORM_DEPENDENT_LIBS  "-framework Carbon;-framework Cocoa;-framework ApplicationServices;-lz")
ENDIF(APPLE)

IF(CYGWIN)
  SET( FLTK2_PLATFORM_DEPENDENT_LIBS ole32; uuid; comctl32; wsock32; supc++; -lm; -lgdi32)
ENDIF(CYGWIN)

#
# Since there is not difference in the include files
# but we need fltk2 we first look for the libraries
#
FIND_LIBRARY(FLTK2_BASE_LIBRARY fltk2
       ${ADDITIONAL_LIBRARY_PATH}
       /usr/lib
       /sw/lib
       ${VISUS_LIBRARIES}
)

# 
# If we have found the library we assume the other 
# libraries can be found in the same places 
#
IF (FLTK2_BASE_LIBRARY)

    FIND_LIBRARY(FLTK2_GLUT_LIBRARY fltk2_glut
       ${ADDITIONAL_LIBRARY_PATH}
       /usr/lib
       /sw/lib
       ${VISUS_LIBRARIES}
    )

    FIND_LIBRARY(FLTK2_GL_LIBRARY fltk2_gl
       ${ADDITIONAL_LIBRARY_PATH}
       /usr/lib
       /sw/lib
       ${VISUS_LIBRARIES}
    )

    #   
    # Finally we look for the include file
    # and hope that we grab the correct one
    # (This should be upgraded with a version 
    # checl)    
    FIND_PATH(FLTK2_INCLUDE_DIR fltk/FL_VERSION.h
       ${ADDITIONAL_INCLUDE_PATH}
        /usr/include
        /sw/include
        ${VISUS_INCLUDE}
    )
    
ELSE (FLTK2_BASE_LIBRARY)
   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("************************************")
      MESSAGE("  Necessary fltk2 files not found")
      MESSAGE("  libfltk2 will be build locally")
      MESSAGE("************************************")
   ENDIF (CMAKE_VERBOSE_MAKEFILE)
   
   #
   # If we have not found the base library we assume
   # we have to build everything ourselfs
   # 

   IF (NOT EXISTS ${VISUS_EXTLIBS}/fltk-2.0)    
   MESSAGE("VISUS_SOURCE_DIR = " ${VISUS_SOURCE_DIR})
   MESSAGE("VISUS_EXTLIBS = " ${VISUS_EXTLIBS})
        
      EXECUTE_PROCESS(                                        
           COMMAND gzip -cd ${VISUS_SOURCE_DIR}/ext-libs/fltk-2.0.tar.gz 
           COMMAND tar xv
           WORKING_DIRECTORY ${VISUS_EXTLIBS}     
      )
                        
      EXECUTE_PROCESS(                                        
           COMMAND mv ${VISUS_EXTLIBS}/fltk-2.0.x-r6101 ${VISUS_EXTLIBS}/fltk-2.0
           WORKING_DIRECTORY ${VISUS_EXTLIBS}     
      )
                        
   ENDIF (NOT EXISTS ${VISUS_EXTLIBS}/fltk-2.0) 
   
   EXECUTE_PROCESS(                                        
      COMMAND ./configure --prefix=${VISUS_EXT_PREFIX}
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/fltk-2.0    
   )

   FOREACH(DIR src OpenGL glut fluid)
      EXECUTE_PROCESS(                                        
         COMMAND make static
         WORKING_DIRECTORY ${VISUS_EXTLIBS}/fltk-2.0/${DIR}
      )
      EXECUTE_PROCESS(                                        
         COMMAND make install
         WORKING_DIRECTORY ${VISUS_EXTLIBS}/fltk-2.0/${DIR}
      )
   ENDFOREACH(DIR)

   FIND_LIBRARY(FLTK2_BASE_LIBRARY fltk2
       ${VISUS_EXT_PREFIX}/lib
       NO_DEFAULT_PATH
   )
   
   FIND_LIBRARY(FLTK2_GL_LIBRARY fltk2_gl
       ${VISUS_EXT_PREFIX}/lib
       NO_DEFAULT_PATH
   )

   FIND_LIBRARY(FLTK2_GLUT_LIBRARY fltk2_glut 
       ${VISUS_EXT_PREFIX}/lib
       NO_DEFAULT_PATH
   )

   FIND_PATH(FLTK2_INCLUDE_DIR fltk/FL_VERSION.h
       ${VISUS_EXT_PREFIX}/include
       NO_DEFAULT_PATH
   )
ENDIF (FLTK2_BASE_LIBRARY)

      


#
# For now we really only use the gl, glut ans basic library. So if the image one 
# did build we don't worry about it
#

IF (FLTK2_BASE_LIBRARY AND FLTK2_GL_LIBRARY AND FLTK2_INCLUDE_DIR)
  SET (FLTK2_FOUND "YES")

  if (FLTK2_GLUT_LIBRARY)
     SET(FLTK2_LIBRARIES ${FLTK2_GLUT_LIBRARY} ${FLTK2_GL_LIBRARY} ${FLTK2_BASE_LIBRARY} )
  else  (FLTK2_GLUT_LIBRARY)
     SET(FLTK2_LIBRARIES ${FLTK2_GL_LIBRARY} ${FLTK2_BASE_LIBRARY} )
  endif  (FLTK2_GLUT_LIBRARY)

  IF(APPLE)
    SET(FLTK2_LIBRARIES ${FLTK2_PLATFORM_DEPENDENT_LIBS} ${FLTK2_LIBRARIES})
  ELSE(APPLE)
    SET(FLTK2_LIBRARIES ${FLTK2_LIBRARIES} ${FLTK2_PLATFORM_DEPENDENT_LIBS})
  ENDIF(APPLE)

  FIND_PATH(FLTK2_FL_INCLUDE_DIR FL
       ${FLTK2_INCLUDE_DIR}/fltk/compat)
  IF (NOT FLTK2_FL_INCLUDE_DIR)
    SET(FLTK2_FL_INCLUDE_DIR ${FLTK2_INCLUDE_DIR}) 
  ENDIF (NOT FLTK2_FL_INCLUDE_DIR)

  IF (CMAKE_VERBOSE_MAKEFILE)
    MESSAGE("Using FLTK2_INCLUDE_DIR = " ${FLTK2_INCLUDE_DIR})
    MESSAGE("Using FLTK2_LIBRARIES = " ${FLTK2_LIBRARIES}) 
    MESSAGE("Using FLTK2_FL_INCLUDE_DIR = " ${FLTK2_FL_INCLUDE_DIR}) 
  ENDIF (CMAKE_VERBOSE_MAKEFILE)

ELSE (FLTK2_BASE_LIBRARY AND FLTK2_GL_LIBRARY AND FLTK2_INCLUDE_DIR)
  SET(FLTK2_FOUND "NO")
ENDIF (FLTK2_BASE_LIBRARY AND FLTK2_GL_LIBRARY AND FLTK2_INCLUDE_DIR)
        

