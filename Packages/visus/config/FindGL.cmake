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



INCLUDE(${CMAKE_ROOT}/Modules/FindOpenGL.cmake)


IF (APPLE_NATIVE_GL)

   # If we are supposed to use the native MAC OS X OpenGl
   # We must make sure that we iunclude the correct headers. This is
   # made slightly more complicated due to the fact that the framework
   # headers are stored under Headers/gl.h rather than under GL/gl.h
   # as normally. To handle this we copy the system headers to
   # VISUS_INCLUDE/GL

   # First we make sure that we found the headers in the first place
   IF (NOT EXISTS ${OPENGL_INCLUDE_DIR}/Headers/gl.h)
      MESSAGE("Build error: native OpenGL headers not found at")
      MESSAGE("OPENGL_INCLUDE_DIR = " ${OPENGL_INCLUDE_DIR})
      SET(OPENGL_FOUND "NO")
   ELSE (NOT EXISTS ${OPENGL_INCLUDE_DIR}/Headers/gl.h)
      IF (NOT EXISTS ${VISUS_INCLUDE}/GL)
         FILE(MAKE_DIRECTORY ${VISUS_INCLUDE}/GL)
      ENDIF (NOT EXISTS ${VISUS_INCLUDE}/GL)
      
      EXEC_PROGRAM(cp 
         ARGS "${OPENGL_INCLUDE_DIR}/Headers/gl.h" "${OPENGL_INCLUDE_DIR}/Headers/glu.h" "${VISUS_INCLUDE}/GL"
      )

      SET(OPENGL_INCLUDE_DIR ${VISUS_INCLUDE})

      FIND_PATH(AGL_INCLUDE_DIR agl.h
         /System/Library/Frameworks/AGL.framework/Headers
         NO_DEFAULT_PATH  
      )
      
   ENDIF (NOT EXISTS ${OPENGL_INCLUDE_DIR}/Headers/gl.h)

ELSE (APPLE_NATIVE_GL)

   # If we are supposed to use the standard OpenGL we first check
   # whetherwe got the correct path (the one containing the GL/
   # prefix)

   IF (NOT EXISTS ${OPENGL_INCLUDE_DIR}/GL/gl.h)
      SET(OPENGL_INCLUDE_DIR OPENGL_INCLUDE_DIR-NOTFOUND)
      FIND_PATH(OPENGL_INCLUDE_DIR GL/gl.h
         /usr/
         /usr/X11R6/
         ${VISUS_INCLUDE}
         PATH_SUFFIXES /include 
      ) 
   ENDIF (NOT EXISTS ${OPENGL_INCLUDE_DIR}/GL/gl.h)

   # IF we are on apple but are not using the native OpenGl we must
   # search for the X version of the libraries since the first find
   # command has likely returned the Frameworks instead

   IF (APPLE)
      SET(OPENGL_glu_LIBRARY OPENGL_glu_LIBRARY-NOTFOUND) 
      FIND_LIBRARY(OPENGL_glu_LIBRARY glu
         /usr
         /usr/X11
         /usr/X11R6
         ${VISUS_LIBRARIES}
         PATH_SUFFIXES /lib        
      ) 
   
      SET(OPENGL_gl_LIBRARY OPENGL_gl_LIBRARY-NOTFOUND) 
      FIND_LIBRARY(OPENGL_gl_LIBRARY gl
         /usr
         /usr/X11
         /usr/X11R6
         ${VISUS_LIBRARIES}
         PATH_SUFFIXES /lib        
      )   
   
      SET(OPENGL_LIBRARIES ${OPENGL_glu_LIBRARY} ${OPENGL_gl_LIBRARY})
   ENDIF (APPLE)
ENDIF (APPLE_NATIVE_GL)
     
# Now do a last check whether we found everything we needed
   
IF (OPENGL_INCLUDE_DIR AND OPENGL_LIBRARIES)   

   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("Using OPENGL_INCLUDE_DIR = " ${OPENGL_INCLUDE_DIR})
      MESSAGE("Using OPENGL_LIBRARIES = " ${OPENGL_LIBRARIES})        
   ENDIF (CMAKE_VERBOSE_MAKEFILE)
 
ELSE (OPENGL_INCLUDE_DIR AND OPENGL_LIBRARIES)   
   SET(OPENGL_FOUND "NO") 
ENDIF (OPENGL_INCLUDE_DIR AND OPENGL_LIBRARIES)   
