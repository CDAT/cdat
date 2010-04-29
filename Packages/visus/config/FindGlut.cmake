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



INCLUDE(${CMAKE_ROOT}/Modules/FindGLUT.cmake)

IF (APPLE_NATIVE_GL)

   IF (NOT EXISTS ${GLUT_INCLUDE_DIR}/glut.h)
      MESSAGE("Build error: native Glut headers not found at")
      MESSAGE("GLUT_INCLUDE_DIR = " ${GLUT_INCLUDE_DIR})
      SET(GLUT_FOUND "NO")
   ELSE (NOT EXISTS ${GLUT_INCLUDE_DIR}/glut.h)
      IF (NOT EXISTS ${VISUS_INCLUDE}/GL)
         FILE(MAKE_DIRECTORY ${VISUS_INCLUDE}/GL)
      ENDIF (NOT EXISTS ${VISUS_INCLUDE}/GL)
      
      EXEC_PROGRAM(cp 
         ARGS "${GLUT_INCLUDE_DIR}/glut.h" "${VISUS_INCLUDE}/GL"
      )

      SET(GLUT_INCLUDE_DIR ${VISUS_INCLUDE})

   ENDIF (NOT EXISTS ${GLUT_INCLUDE_DIR}/glut.h)

ELSEIF (NOT WIN32)

   IF (NOT EXISTS ${GLUT_INCLUDE_DIR}/GL/glut.h)
      SET(GLUT_INCLUDE_DIR GLUT_INCLUDE_DIR-NOTFOUND)
      FIND_PATH(GLUT_INCLUDE_DIR GL/glut.h
         /usr
         /usr/X11R6
         /usr/local
         ${VISUS_INCLUDE}
         PATH_SUFFIXES /include 
      ) 
   ENDIF (NOT EXISTS ${GLUT_INCLUDE_DIR}/GL/glut.h)

   # Now make sure that all necessary glut libraries are found. We
   # need glut Xmu and Xi. The FindGLUT.cmake has already search for
   # all of them but will not display them as missing since they are
   # advanced. Here we simply check whether they are found and if not
   # mark them as non-advanced to force cmake to show them to the user
   
   IF (NOT GLUT_glut_LIBRARY)
      MARK_AS_ADVANCED(CLEAR GLUT_glut_LIBRARY)
   ENDIF (NOT GLUT_glut_LIBRARY)
   
   IF (NOT GLUT_Xmu_LIBRARY)
      MARK_AS_ADVANCED(CLEAR GLUT_Xmu_LIBRARY)
   ENDIF (NOT GLUT_Xmu_LIBRARY)
   
   IF (NOT GLUT_Xi_LIBRARY)
      MARK_AS_ADVANCED(CLEAR GLUT_Xi_LIBRARY)
   ENDIF (NOT GLUT_Xi_LIBRARY)
  
ENDIF (APPLE_NATIVE_GL)

IF (NOT EXISTS ${GLUT_INCLUDE_DIR}/GL/glut.h)
   MESSAGE("Using GLUT_INCLUDE_DIR = " ${GLUT_INCLUDE_DIR})
   MESSAGE("Using GLUT_LIBRARIES = " ${GLUT_LIBRARIES})
   SET(GLUT_FOUND "NO")
   SET(GLUT_INCLUDE_DIR = GLUT_INCLUDE_DIR-NOTFOUND)
ENDIF (NOT EXISTS ${GLUT_INCLUDE_DIR}/GL/glut.h)


IF (GLUT_FOUND)
   IF(CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("Using GLUT_INCLUDE_DIR = " ${GLUT_INCLUDE_DIR})
      MESSAGE("Using GLUT_LIBRARIES = " ${GLUT_LIBRARIES})
   ENDIF(CMAKE_VERBOSE_MAKEFILE)
ENDIF (GLUT_FOUND)
