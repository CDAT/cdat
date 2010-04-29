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


from os.path import splitext,split,join,isfile
from glob import glob

# Function to extract a list of static library names from a CMake environment
# variable. For example, libraries might be "-framework OpenGL;/usr/X11R6/libgl.a"
# and the correct output is ["/usr/X11R6/libgl.a"]
def extractStaticLibs(libraries):
    output = []

    # First we split possible multiple path
    libraries = libraries.split(";")

    for lib in libraries:
        lib = lib.strip()

        ext = splitext(lib)[1]

        #print lib,ext
        if ext == ".a" or ext == ".lib":
            output.append(lib)

    return output
        
        
def extractDynamicLibs(libraries):
    output = []

    # standard search path to look for additional libraries
    search_path = [join("/","usr","X11","lib"),join("/","usr","X11R6","lib")]

    # First we split possible multiple path
    libraries = libraries.split(";")

    for lib in libraries:
        lib = lib.strip()
        ext = splitext(lib)[1]

        if ext == ".a" or ext == ".lib":
            continue

        if lib[0:10] == "-framework": # A frame work we skip
            output += lib.split()            
        elif lib[0:2] == "-l":
            # A -lz or -lgl type flag we expect this to be a dynamic library
            # Since on some systems you cannot be sure that these are found
            # automatically we search for the corresponding file
            
            lib_name = "lib" + lib[2:] + ".*"
            full_lib = []
            for p in search_path:
                full_lib = glob(join(p,lib_name))
                if full_lib != []:
                    output.append(full_lib[0])
                    break
            if full_lib == []:
                output.append(lib)
        elif ext == ".framework":
            output += ["-framework",splitext(split(lib)[1])[0]]
        else:
            output.append(lib)
        
    return output
        
        
def parseMacros(macro_string):
    macro_list = macro_string.split()

    macro_pairs = []
    for m in macro_list:
        m = m[2:] # ignore the -D
        macro_pairs.append(tuple(m.split('=')))

    #print macro_pairs
    return macro_pairs


def touch(module):
  filename = getCPP(module)
  print "Touching %s" % filename
  lines = open(filename).readlines()
  fh = open(filename,"w")
  fh.write("".join(lines))
  fh.close()
  return


def getCPP(module):
  return "%s_wrap.cpp" % module

def getPy(module):
  return "%s.py" % module

def copyModule(module):
  copyIfNewer(getCPP(module))
  copyIfNewer(getPy(module))
  return


def copyIfNewer (file):
  import os, stat
  releaseFile = os.path.join("release", file)
  rmtime = os.stat(releaseFile)[stat.ST_MTIME]

  if os.path.exists(file):
    mtime = os.stat(file)[stat.ST_MTIME]
  else:
    mtime = 0

  if rmtime > mtime:
    import shutil
    print "Copying (%s) to (%s)" % (releaseFile, file)
    shutil.copyfile(releaseFile, file)
  return


def releaseModule(module):
  releaseFile(getCPP(module))
  releaseFile(getPy(module))
  return


def releaseFile(file):
  import os, shutil, filecmp
  releaseFile = os.path.join("release", file)
  if filecmp.cmp(file, releaseFile):
    print "No need to copy (%s) ... file has not changed" % file
  else:
    print "Copying (%s) to (%s)" % (file, releaseFile)
    shutil.copyfile(file, releaseFile)
  return


