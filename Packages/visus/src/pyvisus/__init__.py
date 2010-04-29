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


# This is a global list of references to different scenes created by
# the user in vcdat
gVCDATScenes = []



def loadLibrary(library):
  from ctypes import cdll
  try:
     cdll.LoadLibrary(library)
  except Exception, e:
     print "failed to pre-load dependent shared library:", library, "\n", e

#----------------------------------------------------
#  Pre-load Dependent Visus Shared Libraries
#----------------------------------------------------
try:
  import os
  from ctypes import cdll
  from distutils.sysconfig import get_python_lib
  from pathConfig import VISUS_INTERNAL_LIBRARIES

  dynLibPath = os.path.join(get_python_lib(), "pyvisus")

  for lib in VISUS_INTERNAL_LIBRARIES:
     libname = os.path.join(dynLibPath, os.path.split(lib)[1])
     loadLibrary(libname) 
       
except Exception, e1:
  print "failed to pre-load dependent shared libraries:", e1


def constructArrayFromFile(filename,dim,flip):
    import os.path
    import operator
    import sys
    import numpy

    if not os.path.isfile(filename):
        sys.stderr.write("""\n\nERROR constructDataFromFile:
file %s does not exist\n\n""" % filename)
        return numpy.array([])


    ext = os.path.splitext(filename)[1]
    format = "1"

    if ext == ".bin":
        dtype = numpy.float
    elif ext == ".raw":
        dtype = numpy.uchar
    else:
        sys.stderr.write("""\n\nERROR constructDataFromFile:
file format %s unknown\n\n""" % ext)
        return numpy.array([])


    input = open(filename,'rb')
    a = numpy.fromfile(input,dtype)

    if a.shape[0] != reduce(operator.mul,dim):
        sys.stderr.write("""\n\nERROR constructDataFromFile:
number of elements in the file and dimensions don't match\n\n""")
        return numpy.array([])

    a = numpy.reshape(a,dim)
    if flip == 1:
        a = a.byteswap(True)
    
    return a


try:
  from vcdat_plug import create_visus_tools_menu
except Exception,err:
  print err
  pass
  
