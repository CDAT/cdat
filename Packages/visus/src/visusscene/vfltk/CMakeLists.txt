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

SET(VFLTK_HEADER

  FLTKNodeMenu.h
)

SET(VFLTK_SRC

  FLTKNodeMenu.h
)

INCLUDE_DIRECTORIES(${VISUS_INCLUDE_DIRS})

ADD_LIBRARY(vfltk-static STATIC ${VFLTK_SRC})
SET_TARGET_PROPERTIES(vfltk-static
         PROPERTIES
         OUTPUT_NAME vfltk
         CLEAN_DIRECT_OUTPUT 1	 
         LINK_FLAGS ${VISUS_EXTRA_LINK_FLAGS}
)

IF (VISUS_ENABLE_DYNAMIC)

  ADD_LIBRARY(vfltk-shared SHARED ${VFLTK_SRC})
  TARGET_LINK_LIBRARIES(vfltk-shared ${VISUS_SHARED_LIBS})

  SET_TARGET_PROPERTIES(vfltk-shared
         PROPERTIES
         OUTPUT_NAME vfltk${SO_EXT}
         CLEAN_DIRECT_OUTPUT 1	 
         LINK_FLAGS ${VISUS_EXTRA_LINK_FLAGS}
  )
ENDIF (VISUS_ENABLE_DYNAMIC)
   




