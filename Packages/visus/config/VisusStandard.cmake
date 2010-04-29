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
#########   This file sets the available pre-processor #######
#########   macros to the standard configuration       #######
##############################################################


# 
# Install all of visus including legacy code
# 
SET(VISUS_ENABLE_LEGACY FALSE)

#
# Use IDX interface
#
SET(VISUS_ENABLE_IDX TRUE)


#
# Compile with pthreads for thread support
#
SET(VISUS_ENABLE_PTHREADS TRUE)

#
# Use the FTGL library for font rendering
#
SET(VISUS_ENABLE_FTGL TRUE)


#
# Enable fltk support
#
SET(VISUS_ENABLE_FLTK TRUE)

#
# Set off-screen rendering (NONE,GLX,WGL,OSMESA) 
#
SET(VISUS_OFFSCREEN_RENDERING NONE)

#
# Build the dynamic libraries
#
SET(VISUS_ENABLE_DYNAMIC TRUE)

#
# Build the example and test programs
# 
SET(VISUS_ENABLE_TESTS TRUE)

#
# Build turning on verbose debug statements
#
SET(VISUS_ENABLE_VERBOSE FALSE)


#
# Enable mutex protected atomic counter rather than 
# the assembler version
#
SET(VISUS_ENABLE_HARDWARE_COUNTER FALSE)


#
# Keep track of the node count for debugging purposes
#
SET(VISUS_ENABLE_NODE_COUNT FALSE)

#
# Under Mac OS use the X-windows system rather
# than the frameworkes
#
SET(VISUS_USE_APPLE_X FALSE)

#
# Use the new cmake 2.6 policy to determine linking paths
#
IF (COMMAND cmake_policy)
   cmake_policy(SET CMP0003 NEW) 
ENDIF (COMMAND cmake_policy)


#
# Enable dynamic casting
#
SET(VISUS_USE_DYNAMIC_CAST FALSE)
