#######################################################################
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


import os.path
import os 
import tarfile
import sys

# Test whether we are doing a cdat install
try:
    from cdat_info import externals
except:
    externals = None


visus_path = os.getcwd()

# We want to perform all our building in a special build directory
if not os.path.isdir("build"):
    os.mkdir("build")

cmake_install = False
cmake_exe = None
if externals is not None: # If this is a cdat install
    cmake_exe = "%s/bin/cmake" % externals
    # Try to execute cmake from the externals directory
    i,o,e = os.popen3(cmake_exe + " --version")

    # If there was an error
    if e.readlines() != []:
        cmake_install = True # We need to install our own version
    else: # Otherwise check the version
        version = float(o.readline().split()[2].split("-")[0])
        if version >= 2.4: #if the version is compatible
            cmake_install = False
        else:
            cmake_install = True

# If this is not a cdat build or for some reason the cdat version did
# not work
if cmake_install:
    cmake_exe = "cmake"
    # Try to execute a system cmake 
    i,o,e = os.popen3(cmake_exe + " --version")

    # If there was an error
    if e.readlines() != []:
        cmake_install = True # We need to install our own version
    else: # Otherwise check the version
        version = float(o.readline().split()[2].split("-")[0])
        if version >= 2.4: #if the version is compatible
            cmake_install = False
        else:
            cmake_install = True

# If we could not find a cmake yet we look for our own
if cmake_install:
    cmake_exe = os.path.join(visus_path,"build","bin","cmake")
    # Try to execute a system cmake 
    i,o,e = os.popen3(cmake_exe + " --version")

    # If there was an error
    if e.readlines() != []:
        cmake_install = True # We need to install our own version
    else: # Otherwise check the version
        version = float(o.readline().split()[2].split("-")[0])
        if version >= 2.4: #if the version is compatible
            cmake_install = False
        else:
            cmake_install = True


os.chdir("build")
if cmake_install:
  
  if not os.path.isdir("ext-libs"):
    os.mkdir("ext-libs")
        
  archive = tarfile.open(os.path.join("..","ext-libs","cmake-2.6.0.tar.gz"))
  archive.extractall("ext-libs")

  os.chdir(os.path.join("ext-libs","cmake-2.6.0"))
  os.system("./configure --prefix=%s" % os.path.abspath(os.path.join("..","..")))
  os.system("make install")
  cmake_exe = os.path.join(visus_path,"build","bin","cmake")
  os.chdir(os.path.join(visus_path,"build"))
    
if os.path.isfile("CMakeCache.txt"):
  os.remove("CMakeCache.txt")


cmake_cmd = (cmake_exe
             + " -DADDITIONAL_INCLUDE_PATH=%s" % (os.path.join(externals,"include"))
             + " -DADDITIONAL_LIBRARY_PATH=%s" % (os.path.join(externals,"lib"))
             + " -DCMAKE_BUILD_TYPE=Release"
             + " ..")

if externals:
  cmake_cmd += " -DVISUS_EXT_PREFIX=%s" % externals
  

print cmake_cmd

os.system(cmake_cmd)
    
os.system("make install")

if externals:
    
    os.chdir(os.path.join(visus_path,"src","pyvisus"))
    os.system("%s %s install" % (sys.executable," ".join(sys.argv)))
