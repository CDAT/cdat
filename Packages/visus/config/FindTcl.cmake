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



INCLUDE(${CMAKE_ROOT}/Modules/FindTCL.cmake)

IF (NOT TCL_FOUND)
   FIND_PATH(TCL_CONFIG tclConfig.h
       ${VISUS_LIBRARIES}
       NO_DEFAULT_PATH
   )
   IF (TCL_CONFIG)
      SET(TCL_FOUND "YES")
   ENDIF (TCL_CONFIG)
ELSEIF (CMAKE_VERBOSE_MAKEFILE)
      SET(TCL_CONFIG ${TCL_LIBRARY})
      SET(TK_CONFIG ${TK_LIBRARY})
      MESSAGE("Using TCL_CONFIG = " ${TCL_CONFIG})     
ENDIF (NOT TCL_FOUND)

   
IF (NOT TCL_FOUND)

   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("************************************")
      MESSAGE("  Necessary tcl files not found")
      MESSAGE("  tcl will be build locally")
      MESSAGE("************************************")
   ENDIF (CMAKE_VERBOSE_MAKEFILE)
   
   IF (NOT EXISTS ${VISUS_EXTLIBS}/tcl8.4)    
      EXECUTE_PROCESS(                                        
           COMMAND gzip -cd tcl8.4.9-src.tar.gz 
           COMMAND tar xv
           WORKING_DIRECTORY ${VISUS_EXTLIBS}     
      )
   ENDIF (NOT EXISTS ${VISUS_EXTLIBS}/tcl8.4) 
   
   EXECUTE_PROCESS(                                        
      COMMAND ./configure --prefix=${VISUS_SOURCE_DIR} 
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/tcl8.4/unix     
   )
        
   EXECUTE_PROCESS(                                        
      COMMAND make install
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/tcl8.4/unix        
   )
  
   FIND_PATH(TCL_CONFIG tclConfig.h
       ${VISUS_LIBRARIES}
       NO_DEFAULT_PATH
   )
   
   IF (TCL_CONFIG)
      SET(TCL_FOUND "YES")
      IF (CMAKE_VERBOSE_MAKEFILE)
          MESSAGE("Using TCL_CONFIG = " ${TCL_CONFIG})     
      ENDIF (CMAKE_VERBOSE_MAKEFILE)
   ELSE (TCL_CONFIG)
      MESSAGE("ERROR tcl not found on the system and could not be build")      
   ENDIF (TCL_CONFIG)
         
ENDIF (NOT TCL_FOUND)
        