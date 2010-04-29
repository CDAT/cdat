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

FIND_FILE(VISUS_WIN_DIR visus-windows 
    ${CMAKE_SOURCE_DIR}/..
    C:/
)
IF (NOT EXISTS ${VISUS_WIN_DIR})
    FIND_FILE(VISUS_WIN_DIR windows-libraries 
      C:/
    )
ENDIF (NOT EXISTS ${VISUS_WIN_DIR})

IF (NOT EXISTS ${VISUS_WIN_DIR})
  MESSAGE(FATAL_ERROR "Unable to find windows libraries.  Should either be in visus-windows at same directory level as visus, or in c:/windows-libraries")
ENDIF (NOT EXISTS ${VISUS_WIN_DIR})

SET(ADDITIONAL_INCLUDE_PATH
    ${VISUS_WIN_DIR}/zlib123
    ${VISUS_WIN_DIR}/include
)

SET(ADDITIONAL_LIBRARY_PATH
    ${VISUS_WIN_DIR}/zlib123/projects/visualc6/Win32_LIB_Release
    ${VISUS_WIN_DIR}/lib
)

IF (VISUS_ENABLE_PTHREADS)
    SET(ADDITIONAL_INCLUDE_PATH 
        ${ADDITIONAL_INCLUDE_PATH}
        ${VISUS_WIN_DIR}/include/pthread
        )
ENDIF (VISUS_ENABLE_PTHREADS)

IF (VISUS_ENABLE_FTGL)
    IF (EXISTS ${VISUS_WIN_DIR}/ftgl-2.1.3~rc5)
        SET(VISUS_FTGL_PATH ftgl-2.1.3~rc5)
    ELSE (EXISTS ${VISUS_WIN_DIR}/ftgl-2.1.3~rc5)
        SET(VISUS_FTGL_PATH FTGL)   
    ENDIF (EXISTS ${VISUS_WIN_DIR}/ftgl-2.1.3~rc5)

    IF (EXISTS ${VISUS_WIN_DIR}/freetype-2.3.7)
        SET(VISUS_FREETYPE_PATH freetype-2.3.7)   
    ELSE (EXISTS ${VISUS_WIN_DIR}/freetype-2.3.7)
        SET(VISUS_FREETYPE_PATH freetype-2.3.4)   
    ENDIF (EXISTS ${VISUS_WIN_DIR}/freetype-2.3.7)

    SET(ADDITIONAL_INCLUDE_PATH 
        ${ADDITIONAL_INCLUDE_PATH}
        ${VISUS_WIN_DIR}/${VISUS_FTGL_PATH}/include/
	${VISUS_WIN_DIR}/${VISUS_FREETYPE_PATH}/include
        )
    SET(ADDITIONAL_LIBRARY_PATH 
        ${ADDITIONAL_LIBRARY_PATH}
        ${VISUS_WIN_DIR}/${VISUS_FTGL_PATH}/win32_vcpp/build
	${VISUS_WIN_DIR}/${VISUS_FREETYPE_PATH}/objs
        )
    SET(FREETYPE2_FT_CONFIG ${VISUS_WIN_DIR}/${VISUS_FREETYPE_PATH})
ENDIF (VISUS_ENABLE_FTGL)

IF (VISUS_ENABLE_FLTK)
    SET(ADDITIONAL_INCLUDE_PATH 
        ${ADDITIONAL_INCLUDE_PATH}
        ${VISUS_WIN_DIR}/fltk-2.0.x-r6129
        ${VISUS_WIN_DIR}/fltk-2.0.x-r6101
        )
    SET(ADDITIONAL_LIBRARY_PATH 
        ${ADDITIONAL_LIBRARY_PATH}
        ${VISUS_WIN_DIR}/fltk-2.0.x-r6129/lib
        ${VISUS_WIN_DIR}/fltk-2.0.x-r6101/lib
        )
ENDIF (VISUS_ENABLE_FLTK)

IF (VISUS_ENABLE_LEGACY) 
    SET(ADDITIONAL_INCLUDE_PATH 
        ${ADDITIONAL_INCLUDE_PATH}
	${VISUS_WIN_DIR}/fox-1.7.9/include
	${VISUS_WIN_DIR}/include/libxml2
        )
    SET(ADDITIONAL_LIBRARY_PATH 
        ${ADDITIONAL_LIBRARY_PATH}
	${VISUS_WIN_DIR}/fox-1.7.9/lib
        )
ENDIF (VISUS_ENABLE_LEGACY) 

#====================================================
#  Add Windows Paths to Overall CMAKE Search Paths        
#====================================================
SET(CMAKE_INCLUDE_PATH 
        ${CMAKE_INCLUDE_PATH}
        ${ADDITIONAL_INCLUDE_PATH}
)

SET(CMAKE_LIBRARY_PATH 
        ${CMAKE_LIBRARY_PATH}
        ${ADDITIONAL_LIBRARY_PATH}
        
)

SET (GLUT_glut_LIBRARY ${VISUS_WIN_DIR}/lib/glut32.lib )

