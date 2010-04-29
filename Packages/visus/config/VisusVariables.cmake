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


##############################################################
#########   This file defines the all encompassing vars ###### 
#########   VISUS_INCLUDE_DIRS  VISUS_SHARED_LIBS      ####### 
#########   and VISUS_STATIC_LIBS                      #######
#########                                              #######
#########   Used in visusscene/CMakeLists.txt as well  #######
#########   external programs which use visus          #######
##############################################################

MACRO(LoadVisusVariables)
IF (VISUS_ENABLE_FTGL) 
   IF (NOT EXISTS ${VISUS_INCLUDE}/Fonts)    
       EXECUTE_PROCESS(                                        
            COMMAND gzip -cd ${VISUS_SOURCE_DIR}/ext-libs/fonts.tar.gz 
            COMMAND tar xv
            WORKING_DIRECTORY ${VISUS_INCLUDE}     
       )       
   ENDIF (NOT EXISTS ${VISUS_INCLUDE}/Fonts)
ENDIF (VISUS_ENABLE_FTGL) 

SET (VISUS_INCLUDE_DIRS
     ${VISUS_SRC}/glew/GL
     ${VISUS_SRC}/libcontour
     ${VISUS_SRC}/xmlParser
     ${VISUS_SRC}/visusscene

     ${VISUS_RENDERER_INCLUDE_DIR}
     ${OPENGL_INCLUDE_DIR}
     ${GLUT_INCLUDE_DIR}
)

SET (VISUS_SHARED_LIBS
     visusscene-shared
     glew-shared  
     contour-shared 
     xmlParser-shared

     ${OPENGL_LIBRARIES}
     ${GLUT_LIBRARIES}
     ${VISUS_EXTRA_LIBS}
)

SET (VISUS_STATIC_LIBS
     visusscene-static    
     glew-static  
     contour-static
     xmlParser-static 

     ${OPENGL_LIBRARIES}
     ${GLUT_LIBRARIES}
)

IF (VISUS_ENABLE_IDX)
   SET (VISUS_INCLUDE_DIRS
       ${VISUS_INCLUDE_DIRS}
       ${VISUS_SRC}/visus3d
   )
   SET (VISUS_SHARED_LIBS
       ${VISUS_SHARED_LIBS}
       visus3d-shared
       ${ZLIB_LIBRARIES}
   )
   SET (VISUS_STATIC_LIBS
       ${VISUS_STATIC_LIBS}
       visus3d-static
       ${ZLIB_LIBRARIES}
   )
ENDIF (VISUS_ENABLE_IDX)
       
IF (VISUS_ENABLE_PTHREADS)
   SET (VISUS_INCLUDE_DIRS
       ${VISUS_INCLUDE_DIRS}
       ${PTHREAD_INCLUDE_DIR}
   )
   SET (VISUS_SHARED_LIBS
       ${VISUS_SHARED_LIBS}
       ${PTHREAD_LIBRARIES}
   )
   SET (VISUS_STATIC_LIBS
       ${VISUS_STATIC_LIBS}
       ${PTHREAD_LIBRARIES}
   )
ENDIF (VISUS_ENABLE_PTHREADS)
    
IF (VISUS_ENABLE_FTGL)
   SET (VISUS_INCLUDE_DIRS
       ${VISUS_INCLUDE_DIRS}
       ${FTGL_INCLUDE_DIR}
       ${FREETYPE2_INCLUDE_DIR}
       ${FREETYPE2_FT2BUILD}
   )
   SET (VISUS_SHARED_LIBS
       ${VISUS_SHARED_LIBS}
       ${FTGL_LIBRARIES}
       ${FREETYPE2_LIBRARIES}
   )
   SET (VISUS_STATIC_LIBS
       ${VISUS_STATIC_LIBS}
       ${FTGL_LIBRARIES}
       ${FREETYPE2_LIBRARIES}
   )
ENDIF (VISUS_ENABLE_FTGL)

IF (VISUS_ENABLE_FLTK)
   SET (VISUS_INCLUDE_DIRS
       ${VISUS_INCLUDE_DIRS}
       ${FLTK2_INCLUDE_DIR}
   )
   SET (VISUS_SHARED_LIBS
       ${VISUS_SHARED_LIBS}
       ${FLTK2_LIBRARIES}
   )
   SET (VISUS_STATIC_LIBS
       ${VISUS_STATIC_LIBS}
       ${FLTK2_LIBRARIES}
   )
ENDIF (VISUS_ENABLE_FLTK)

ENDMACRO(LoadVisusVariables INCLUDE_VISUSSCENE)

