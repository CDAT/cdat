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
# Try to find libpthread.

FIND_PATH(PTHREAD_INCLUDE_DIR pthread.h
     /usr/include
     ${VISUS_INCLUDE}   
)
        
IF (NOT WIN32)

FIND_LIBRARY(PTHREAD_LIBRARIES pthread
     /usr/lib
     ${VISUS_LIBRARIES}   
)

ELSE (NOT WIN32)

FIND_LIBRARY(PTHREAD_LIBRARIES pthreadVCE
     /usr/lib
     ${VISUS_LIBRARIES}   
)

ENDIF (NOT WIN32)



IF (PTHREAD_LIBRARIES AND PTHREAD_INCLUDE_DIR)
      SET(PTHREAD_FOUND "YES")
      IF (CMAKE_VERBOSE_MAKEFILE)
          MESSAGE("Using PTHREAD_INCLUDE_DIR = " ${PTHREAD_INCLUDE_DIR})
	  MESSAGE("Using PTHREAD_LIBRARIES = " ${PTHREAD_LIBRARIES}) 
      ENDIF (CMAKE_VERBOSE_MAKEFILE)
ELSE (PTHREAD_LIBRARIES AND PTHREAD_INCLUDE_DIR)
      
      SET(PTHREAD_FOUND "NO")
ENDIF (PTHREAD_LIBRARIES AND PTHREAD_INCLUDE_DIR)
