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


from numpy.distutils.core import setup, Extension
from distutils.sysconfig import get_python_lib
import os
from os.path import getmtime, exists, join, abspath,isdir
import sys

from pathConfig import *

DEBUG="Debug"
RELEASE="Release"

#gPythonLibs = { DEBUG:PYTHON_DEBUG_LIBRARY,
#		RELEASE:PYTHON_LIBRARY
#		}

if sys.argv[0].find("python_d") >= 0:
  gPythonVersion = DEBUG 
else:
  gPythonVersion = RELEASE 


PYTHON_SITE_PACKAGE = [ get_python_lib() ]
VISUS_TOP_PACKAGE = "pyvisus"
PYVISUS_LIB_PATH = join(PYTHON_SITE_PACKAGE[0], VISUS_TOP_PACKAGE)


def patchWrapFile(swigFile,wrapFile):
  #------------------------------------------------------
  # Patch for SWIG Bug
  # Although this SWIG code should work, compiler barfs on it, so
  # patch in code that does work.  Namely, use operator-> instead
  # of operator*
  #------------------------------------------------------
  patchApplied = 0

  smartRef1Left = " &_result_ref ="
  smartRef1Right= "arg1)->operator *();"
  smartRef2 = "result = (TYPE *) &_result_ref;"
  smartRefUse = None
  smartRefType = None
  printOn = 0

  print ""

  # For now it seems every compiler needs this fix
  if 1: # AND CMAKE_CXX_COMPILER in ["cl"]: 
    found = 0
    lines = open(wrapFile).readlines()
    of = open(wrapFile, "w")
    for line in lines:
 
      sline = line.strip()
      indxLeft = sline.find(smartRef1Left)
      indxRight= sline.find(smartRef1Right)
      if (indxLeft > 0) and (indxRight > 0):
        smartRefType = sline[0:indxLeft]
        indxSpace = smartRefType.find(" ")
        if indxSpace > 0:
           smartRefType = smartRefType[0:indxSpace].strip()
	smartRefUse = smartRef2.replace("TYPE", smartRefType)
        print "Found SWIG bug... type(%s) fixing in file (%s)" % (smartRefType, wrapFile)
        of.write("//%s" % line)
      elif sline == smartRefUse:
        of.write("//%s" % line)
        found = 1
      else:
	of.write(line);

      if found:
        of.write("result = (%s *) (arg1)->operator-> ();\n" % smartRefType);
	patchApplied = 1
        found = 0
    of.close()

  if patchApplied:
    print "Smartpointer use was patched in file... %s" % wrapFile
  return


def winPatchPath(inPath):
  global gPythonVersion
  path = inPath.replace("$(OutDir)", gPythonVersion).replace("/", "\\")
  return path 


def winPatchLibraries():
  #------------------------------------------------------
  # Patch For Windows MSVC V9.0 Compiler
  # This ensures library paths are correct for link with
  # distutils usage.
  #------------------------------------------------------
  global VISUS_INTERNAL_LIBRARIES, VISUS_STATIC_LIBRARIES, CMAKE_CXX_COMPILER, VISUS_LIBRARY_DIR, VISUS_DYNAMIC_LIBRARIES, gPythonVersion
  #, gPythonLibs

  if CMAKE_CXX_COMPILER in ["cl"]:
     VISUS_LIBRARY_DIR[0] = "%s%s%s" % (VISUS_LIBRARY_DIR[0], os.sep, gPythonVersion)

     # Patch Internal Library Names
     internalLibs = VISUS_INTERNAL_LIBRARIES
     VISUS_INTERNAL_LIBRARIES = []
     for lib in internalLibs:
        VISUS_INTERNAL_LIBRARIES.append(winPatchPath(lib))

     # Split Static Libraries Into Libpaths and Libraries
     staticLibs = VISUS_STATIC_LIBRARIES
     VISUS_STATIC_LIBRARIES = [] 
     for inLib in staticLibs:
       lib = winPatchPath(inLib).replace(".dll", ".lib")
       (path, libname) = os.path.split(lib)
       VISUS_STATIC_LIBRARIES.append(libname)
       if len(path) > 0:
	 try:
	   ndx = VISUS_LIBRARY_DIR.index(path) 
         except:
           VISUS_LIBRARY_DIR.append(path)

     # Ensure Dynamic Libraries Have Extension
     dynamicLibs = VISUS_DYNAMIC_LIBRARIES
     VISUS_DYNAMIC_LIBRARIES = [] 
     for lib in dynamicLibs:
        (name,ext) = os.path.splitext(lib)
	if len(ext) == 0:
	   lib = lib + ".lib"
	VISUS_DYNAMIC_LIBRARIES.append(lib)

     # Required Windows Libraries
     VISUS_DYNAMIC_LIBRARIES.append("glew32.lib")
     VISUS_DYNAMIC_LIBRARIES.append("ole32.lib")
     VISUS_DYNAMIC_LIBRARIES.append("user32.lib")
     VISUS_DYNAMIC_LIBRARIES.append("gdi32.lib")
     VISUS_DYNAMIC_LIBRARIES.append("shell32.lib")
     VISUS_DYNAMIC_LIBRARIES.append("Advapi32.lib")
     VISUS_DYNAMIC_LIBRARIES.append("Ws2_32.lib")

     # Put Python Library On Build Path
     #VISUS_LIBRARY_DIR.append(gPythonLibs[gPythonVersion])

  return


def buildSWIGLibraries(includes, modules, buildModules=None):
  #------------------------------------------------------
  # Runs SWIG on all Python packages 
  #------------------------------------------------------
  from smartPointerTypeMaps import SmartPointerTypeMaps, includeFile, outputFile, functionFile

  if buildModules is None:
    buildModules = modules

  swiginc = ""
  for d in includes:
     swiginc += " -I" + d
     
  # Create SWIG TypeMap Files
  outF = outputFile() 
  incF = includeFile() 
  funF = functionFile() 

  # Generate Typemaps for each Smart Pointer base type
  for base in VISUS_SP_BASES:
    typeMap = SmartPointerTypeMaps(base)
    for module in modules:
      typeMap.process("%s.i" % module)
    typeMap.write(incF,funF,outF)

  # Close TypeMap Files
  outF.close()
  incF.close()
  funF.close()

  # Generate SWIG C++ File
  for module in buildModules:
     buildSWIGFile(swiginc, module)

  return


def buildSWIGFile(swigInc, module):
  #------------------------------------------------------
  # Run SWIG on selected Python packages 
  #------------------------------------------------------
  swigFile = "%s.i" % module
  wrapFile = "%s_wrap.cpp" % module

  # Generate Wrap File
  print "\n *** Running swig to create code for module ... %s\n" % module
  cmd = "swig -python %s -c++ -shadow -o %s %s " % (swigInc, wrapFile, swigFile)
  print cmd
  rc = os.system(cmd)
  if rc:
    raise RuntimeError("Error running SWIG for file (%s)" % swigFile)

  # Patch Is Necessary Due To Bug In Generated Code
  patchWrapFile(swigFile,wrapFile)
  return


#==============================================================================
#==============================================================================
#============     M A I N    ==================================================
#==============================================================================
#==============================================================================

winPatchLibraries()

#----------------------------------
# Interface Files To Build
#----------------------------------
PYTHON_SWIG_PACKAGES = ["numpyconversion"]
PYTHON_SWIG_PACKAGES.extend( VISUS_PY_PACKAGES )

#----------------------------------
# For now we call swig on our own since there still exists a bug
# preventing us from calling it with '-c++' (ptb)
#----------------------------------
if sys.argv[1] == "swig":
  if len(sys.argv) > 2:
    buildSWIGLibraries( VISUS_INCLUDES, PYTHON_SWIG_PACKAGES, sys.argv[2:]) 
  else:
    buildSWIGLibraries( VISUS_INCLUDES, PYTHON_SWIG_PACKAGES ) 
  print "\n\nSWIG code has been successfully generated... now you can build and install"
  sys.exit(0)

elif sys.argv[1] == "clean":
  from setupSupport import touch
  if len(sys.argv) == 3:
    touch(sys.argv[2]) 
    sys.exit(0)
  else: 
    for package in PYTHON_SWIG_PACKAGES:
      touch(package) 

elif sys.argv[1] == "release":
  from setupSupport import releaseModule
  for module in PYTHON_SWIG_PACKAGES:
    releaseModule(module)
  releaseFile("VisusSmartPointerTypeMaps.h")
  releaseFile("VisusSmartPointerTypeMaps.i")
  releaseFile("VisusSmartPointerTypeMapsFuncs.i")

  sys.exit(0)

elif sys.argv[1] == "env":
  print "VisusIncDir      : ",VISUS_INCLUDES
  print "VisusStaticLibs  : ",VISUS_STATIC_LIBRARIES
  print "VisusDynamicLibs : ", VISUS_DYNAMIC_LIBRARIES
  print "VisusLibDir: ",VISUS_LIBRARY_DIR
  print "VisusLibs: ",VISUS_LIBRARIES
  print "VisusDefines :", VISUS_COMPILE_FLAGS
  print "Python-Site-Package:",PYTHON_SITE_PACKAGE
  sys.exit(0)



#--------------------------------------------------
# Copy Modules If Release Is Newer
#--------------------------------------------------
from setupSupport import copyModule, getCPP
for module in PYTHON_SWIG_PACKAGES:
  copyModule(module)
copyIfNewer("VisusSmartPointerTypeMaps.h")
copyIfNewer("VisusSmartPointerTypeMaps.i")
copyIfNewer("VisusSmartPointerTypeMapsFuncs.i")

#---------------------------------------------
# Determine Dynamic Library Installation Path 
#---------------------------------------------
dynLibPath = PYVISUS_LIB_PATH
if CMAKE_CXX_COMPILER in ["cl"]:
  # Windows
  #dynLibPath = "c:\\windows\\system32"
  PYVISUS_RUNTIME_PATH = None
else:
  PYVISUS_RUNTIME_PATH = [ dynLibPath ]

print "** Dynamic internal library installation path:", dynLibPath
print "** Runtime paths:", PYVISUS_RUNTIME_PATH

#----------------------------------
# Determine the absolute path to the site-package/pyvisus/lib directory
#----------------------------------
PYTHON_MODULE_NAMES = [ VISUS_TOP_PACKAGE ]
PYTHON_MODULES = []

PYTHON_MODULES.append(Extension('%s._numpyconversion'%VISUS_TOP_PACKAGE,
                                sources = ['numpyconversion.cpp',
                                           getCPP("numpyconversion")],
                                include_dirs = PYTHON_SITE_PACKAGE + VISUS_INCLUDES,
                      ))

#--------------------------------------------------
# Build the Visus Packages as python extensions 
#--------------------------------------------------
for module in VISUS_PY_PACKAGES:
   PYTHON_MODULES.append(Extension('%s._%s'%(VISUS_TOP_PACKAGE, module),
                                   language='c++',
                                   sources = [getCPP(module)],
                                   include_dirs = PYTHON_SITE_PACKAGE + VISUS_INCLUDES,
                                   library_dirs = VISUS_LIBRARY_DIR,
                                   runtime_library_dirs = PYVISUS_RUNTIME_PATH,
                                   extra_objects = VISUS_STATIC_LIBRARIES,
                                   extra_link_args = VISUS_DYNAMIC_LIBRARIES,
                                   define_macros = VISUS_COMPILE_FLAGS,
                                   undef_macros = [ "NDEBUG" ],
                                  ))

#----------------------------------------
# Build It!!
#----------------------------------------
setup (name = VISUS_TOP_PACKAGE,
       version = 'alpha0.1',
       description = 'Python wrapper around the ViSUS library ',
       author = "ViSUS LDRD Visualization Project, LLNL",
       author_email = "pascucci@llnl.gov",
       url = "http://www.llnl.gov/casc/people/pascucci",
       packages = PYTHON_MODULE_NAMES,
       package_dir = {VISUS_TOP_PACKAGE : '.'},
       data_files=[(dynLibPath, VISUS_INTERNAL_LIBRARIES)],
       ext_modules=PYTHON_MODULES,
       )

if sys.argv[1] == "--help":
  print "\n".join([
                   "ViSUS specific options:",
                   "  setup.py swig      run SWIG on interface files and generate python binding code",
                   "  setup.py release   copy SWIG generated python binding code into release/ subdirectory for CVS checkin",
                   "  setup.py env       prints environment settings of Visus libraries, defines, includes, etc",
                   "",
                  ])

