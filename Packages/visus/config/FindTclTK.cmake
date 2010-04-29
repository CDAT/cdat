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



# Try to find some system paths

INCLUDE(${CMAKE_ROOT}/Modules/FindTCL.cmake)

IF (TCL_FOUND)
   # If we found some system tcl stuff we try to find 
   # all that we need

   # we need the tclConfig which should be with the libraries 
   FIND_PATH(TCL_CONFIG tclConfig.h
        ${VISUS_LIBRARIES}
        ${TCL_LIBRARY}
        NO_DEFAULT_PATH
   )
  
   # However we also need the private headers
   FIND_PATH(TCL_PRIVATE_INCLUDE tclInt.h
        ${VISUS_SOURCE_DIR}/ext-libs/tcl8.4.9/generic
        ${TCL_LIBRARY}
        ${TCL_INCLUDE_PATH}
        ${TCL_TCLSH}
        NO_DEFAULT_PATH
   )
ENDIF (TCL_FOUND)
#
# NOT FULLY TESTED ... IT WAS NO LONGER NEEDED
#
#

        
# IF we found both the config and the private 
# includes we are done

IF (TCL_CONFIG AND TCL_PRIVATE_INCLUDE)
  IF (CMAKE_VERBOSE_MAKEFILE)
     MESSAGE("Using TCL_CONFIG = " ${TCL_CONFIG})
     MESSAGE("Using TCL_PRIVATE_INCLUDE = " ${TCL_PRIVATE_INCLUDE})
  ENDIF (CMAKE_VERBOSE_MAKEFILE)
ELSE (TCL_CONFIG AND TCL_PRIVATE_INCLUDE)
 
   # In principle we could check whether we found the libraries 
   # just not the internal headers. However, it seems to be a bad 
   # idea to link against a different systems library. So if either
   # is missing we build our own

   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("************************************")
      MESSAGE("  Necessary TCL files not found")
      MESSAGE("  TCL_CONFIG = " ${TCL_CONFIG})
      MESSAGE("  TCL_PRIVATE_INCLUDE = " ${TCL_PRIVATE_INCLUDE})
      MESSAGE("  tcl will be build locally")
      MESSAGE("************************************")
   ENDIF (CMAKE_VERBOSE_MAKEFILE)
   
   IF (NOT EXISTS ${VISUS_EXTLIBS}/tcl8.4.9)    
      EXECUTE_PROCESS(                                        
           COMMAND gzip -cd tcl8.4.9-src.tar.gz 
           COMMAND tar xv
           WORKING_DIRECTORY ${VISUS_EXTLIBS}     
      )
   ENDIF (NOT EXISTS ${VISUS_EXTLIBS}/tcl8.4.9)    
 

   EXECUTE_PROCESS(                                        
      COMMAND ./configure --prefix=${VISUS_SOURCE_DIR} 
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/tcl8.4.9/unix  
   )
        
   EXECUTE_PROCESS(                                        
      COMMAND make install
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/tcl8.4.9/unix   
   )
   
   # Now we reset the variables to the local build

   FIND_PATH(TCL_CONFIG tclConfig.h
        ${VISUS_LIBRARIES}
        NO_DEFAULT_PATH
   )
  
   FIND_PATH(TCL_PRIVATE_INCLUDE tclInt.h
        ${VISUS_SOURCE_DIR}/ext-libs/tcl8.4.9/generic
        NO_DEFAULT_PATH
   )
   
   IF (TCL_CONFIG AND TCL_PRIVATE_INCLUDE)
      IF (CMAKE_VERBOSE_MAKEFILE)
         MESSAGE("Using TCL_CONFIG = " ${TCL_CONFIG})
         MESSAGE("Using TCL_PRIVATE_INCLUDE = " ${TCL_PRIVATE_INCLUDE}) 
      ENDIF (CMAKE_VERBOSE_MAKEFILE)
   ELSE (TCL_CONFIG AND TCL_PRIVATE_HEADERS)
      MESSAGE("ERROR tcl not found on the system and could not be build")
   ENDIF (TCL_CONFIG AND TCL_PRIVATE_HEADERS)
ENDIF (TCL_CONFIG AND TCL_PRIVATE_INCLUDE)


#
# Now try exactly the same with Tk
#
IF (TCL_FOUND)
   FIND_PATH(TK_CONFIG tkConfig.h
        ${TK_LIBRARY}
        NO_DEFAULT_PATH
   )
ENDIF (TCL_FOUND)
        
# IF we found both the config and the private 
# includes we are done

IF (TK_CONFIG)
  IF (CMAKE_VERBOSE_MAKEFILE)
     MESSAGE("Using TK_CONFIG = " ${TK_CONFIG})
  ENDIF (CMAKE_VERBOSE_MAKEFILE)
ELSE (TK_CONFIG)
 
   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("************************************")
      MESSAGE("  Necessary TK files not found")
      MESSAGE("  TK_CONFIG = " ${TK_CONFIG})
      #MESSAGE("  tk will be build locally")
      MESSAGE("************************************")
   ENDIF (CMAKE_VERBOSE_MAKEFILE)
ENDIF (TK_CONFIG)

