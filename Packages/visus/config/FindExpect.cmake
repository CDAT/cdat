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
# THIS IS UNTESTED CODE .... IT IS NO LONGER NEEDED
#

#
# Check whether expect.h is installed
#

FIND_FILE(EXPECT_INCLUDE_DIR expect.h
     ${VISUS_INCLUDE}
     NO_DEFAULT_PATH                  
)

#
# And whether the library is compiled and in the correct place
#

FIND_LIBRARY(EXPECT_LIBRARIES expect5.43
     ${VISUS_LIBRARIES}
     NO_DEFAULT_PATH                  
)
IF (EXPECT_INCLUDE_DIR AND EXPECT_LIBRARIES)
      MESSAGE("Using EXPECT_INCLUDE_DIR = " ${EXPECT_INCLUDE_DIR}) 
      MESSAGE("Using EXPECT_LIBRARIES = " ${EXPECT_LIBRARIES}) 
ELSE (EXPECT_INCLUDE_DIR AND EXPECT_LIBRARIES)
  
   IF (CMAKE_VERBOSE_MAKEFILE)
      MESSAGE("************************************")
      MESSAGE("  Necessary libexpect files not found")
      MESSAGE("  EXPECT_INC = " ${EXPECT_INC})
      MESSAGE("  EXPECT_LIBRARIES = " ${EXPECT_LIBRARIES})
      MESSAGE("  libexpect will be build locally")
      MESSAGE("************************************")
   ENDIF (CMAKE_VERBOSE_MAKEFILE)
   
   IF (NOT EXISTS ${VISUS_EXTLIBS}/expect-5.43)    
      EXECUTE_PROCESS(                                        
           COMMAND gzip -cd ${VISUS_SOURCE_DIR}/ext-libs/expect-5.43.tar.gz 
           COMMAND tar xv
           WORKING_DIRECTORY ${VISUS_EXTLIBS}     
      )
   ENDIF (NOT EXISTS ${VISUS_EXTLIBS}/expect-5.43)      
   
   EXECUTE_PROCESS(                                        
      COMMAND ./configure --prefix=${VISUS_BINARY_DIR} --with-tcl=${TCL_CONFIG} --with-tk=${TK_CONFIG} --with-tclinclude=${TCL_PRIVATE_INCLUDE}
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/expect-5.43     
   )
        
   EXECUTE_PROCESS(                                        
      COMMAND make install
      WORKING_DIRECTORY ${VISUS_EXTLIBS}/expect-5.43    
   )

   FIND_FILE(EXPECT_INCLUDE_DIR expect.h
     ${VISUS_INCLUDE}
     NO_DEFAULT_PATH                  
   )

   FIND_LIBRARY(EXPECT_LIBRARIES expect5.43
       ${VISUS_LIBRARIES}
       NO_DEFAULT_PATH
   )

   IF (EXPECT_INCLUDE_DIR AND EXPECT_LIBRARIES)
      IF (CMAKE_VERBOSE_MAKEFILE)
            MESSAGE("Using EXPECT_INCLUDE_DIR = " ${EXPECT_INCLUDE_DIR}) 
            MESSAGE("Using EXPECT_LIBRARIES = " ${EXPECT_LIBRARIES}) 
      ENDIF (CMAKE_VERBOSE_MAKEFILE)
   ELSE (EXPECT_INCLUDE_DIR AND EXPECT_LIBRARIES)
      MESSAGE("ERROR expect not found on the system and could not be build")
   ENDIF (EXPECT_INCLUDE_DIR AND EXPECT_LIBRARIES)
   
ENDIF (EXPECT_INCLUDE_DIR AND EXPECT_LIBRARIES)
