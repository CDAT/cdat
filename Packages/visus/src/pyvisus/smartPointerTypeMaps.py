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


##############################################################################
#
#  SmartPointerTypeMaps is the module which will generate
#  the type maps appropriate to transform VisusSmartPointer< DerivedType >
#  to VisusSmartPointer< BaseType >
#
#  Since the VSP< DT > and VSP< BT > are not in the same inheritance tree this
#  is outside the bounds of auto type-casting
#
##############################################################################

baseFileName = "VisusSmartPointerTypeMaps%s"
includeFileName = baseFileName % ".h"
outputFileName = baseFileName % ".i"
functionFileName = baseFileName % "Funcs.i"

class SmartPointerTypeMaps:

  def __init__(self,base):
    self.sp   = "p%s" % base
    self.base = base
    self.requiredImport = "%s.i" % base
    self.classes = []

    self.inName="inSP%s" % base
    self.inProto="int %s(PyObject* obj, %s& outPtr)" % (self.inName, self.sp)

    self.inFunc= [self.inProto,
                  "{"
                  "  /* Auto-generated Typemap Code */",
                  "  void *pVoid=0;",
                  "",
                  ]

    self.ckName="checkSP%s" % base
    self.ckProto="void %s(PyObject* obj, int& outPtr)" % self.ckName
    self.ckFunc= [self.ckProto,
                  "{",
                  "  /* Auto-generated Typemap Code */",
                  "  void *pVoid=0;",
                  "",
                  ]
    
    self.saveClass(self.base, self.sp)

    self.inMap = ["%%typemap(in) %s" % self.sp, 
                  "{",
                  "  if (! %s( $input, $1 ))" % self.inName,
		  "    SWIG_fail; ",
		  "}",
                  "%%typemap(in) const %s &" % self.sp, 
                  "{",
                  "  if (! %s( $input, (* $1) ))" % self.inName,
		  "    SWIG_fail; ",
		  "}",
		 ]
		  
    self.ckMap = ["%%typemap(typecheck) %s" % self.sp,
                  "{",
		  "  %s ($input, $1 );" % self.ckName,
		  "}",
                  "%%typemap(typecheck) const %s &" % self.sp,
                  "{",
		  "  %s ($input, $1 );" % self.ckName,
		  "}",
                 ]
    
    self.includes = []
    return

  def process(self,swigFile):
    #=========================================================
    # Process the given SWIG file and it's %includes to see 
    # if contains creation of SmartPointer of our base class 
    #=========================================================
    template = "WrapSmartPointer"
    begPy = "("
    endPy = ")"
    smartPtr = "VisusSmartPointer"
    separator = ","
    include = "%include \""
    cInclude = "#include \""
    endInclude = "\""
    imp = "%import"
    group = self.requiredImport 

    print "****** Creating typemaps file for %s search module %s ******" % (self.base, swigFile)

    derivedTypes = [self.base]
    swigTypes = {}
    swigTypes[self.base] = self.sp

    localIncludes=[]

    # Read Main SWIG Interface File
    lines = open(swigFile).readlines()
    for line in lines:
      idx = line.find(include)
      endIdx = line.find(endInclude,idx+len(include)+1)

      # This Is An Included SWIG Interface File
      if idx >= 0 and endIdx >= 0:
	otherFile = line[idx+len(include):endIdx]

	# Read Other SWIG Interface File
        otherLines = open(otherFile).readlines()

	foundGroup = 0

        # Process Lines And See If Is-A VisusGroup and Has-A SmartPointer
	for line in otherLines:

	   if line.find(template) >=0 and foundGroup: 
	     idxBegPy = line.find(begPy)
	     idxEndPy = line.find(endPy)
             idxSep = line.find(separator)
	     pythonType= line[idxBegPy+1:idxSep].strip()
	     cppType = line[idxSep+1:idxEndPy].strip()
	     #print "Found SmartPointer base %s ... Python %s" % (cppType, pythonType)
	     derivedTypes.append(cppType)
	     swigTypes[cppType] = pythonType

	   elif line.find(imp) >= 0 and line.find(group) >= 0:
	     foundGroup = 1

           else:
             cIdx = line.find(cInclude)
	     endCIdx = line.find(endInclude,cIdx+len(cInclude)+1)
	     if cIdx >=0 and endCIdx > cIdx:
                includeFile = line[cIdx+len(cInclude):endCIdx]
                localIncludes.append(includeFile) 

    #  Add necessary include files
    if len(derivedTypes) > 0:
      for include in localIncludes:
        if include not in self.includes:
           self.includes.append(include)

    #  Add appropriate classes
    for dT in derivedTypes:
      if swigTypes[dT] not in self.classes:
        self.saveClass(dT, swigTypes[dT])

    return

  def ifClause(self,clauseElse,cppT):
    return ["  /* Convert To %s */" % cppT,
            " %s if (SWIG_ConvertPtr(obj,(void**) &pVoid," % clauseElse,
            "           SWIG_TypeQuery(\"VisusSmartPointer<%s> *\")," % cppT,
            "           SWIG_POINTER_EXCEPTION) != SWIG_ERROR) {",
            ]

  def saveClass(self,cppT,swigT):
    # Determine if First or Else Required
    clauseElse = " else"
    if len(self.classes) == 0:
      clauseElse = ""

    # Add Class To Type Maps
    print "SmartPointer base %s ... Python %s" % (cppT, swigT)
    self.classes.append( swigT )

    # Generate IN Type Map Code
    self.inFunc.extend(self.ifClause(clauseElse,cppT))
    self.inFunc.extend(["    %s *ptr=reinterpret_cast< VisusSmartPointer< %s > * >(pVoid);" % (swigT, cppT),
                        "    outPtr = (*ptr);",
                        "  }"
                        ])

    # Generate Check Type Map Code
    self.ckFunc.extend(self.ifClause(clauseElse,cppT))
    self.ckFunc.extend(["    outPtr = 1;",
                        "  }",
			])

    return

  def write(self,incFile,funcFile,outFile):
    global includeFileName

    # Add Else Condition on "in" typemaps 
    self.inFunc.extend(["  else {",
                        "     PyErr_SetString(PyExc_TypeError, \"%s expected\");" % " or ".join(self.classes),
                        "     return 0;",
                        "  }",
			"  return 1;",
                        "}",
			])

    # Add Else Condition on "check" typemaps 
    self.ckFunc.extend(["  else {",
                        "    outPtr = 0;",
                        "  }",
                        "}",
			])

    # Write To Include File
    incFile.write("\n\n")
    for include in self.includes:
      incFile.write("#include \"%s\"\n" % include)
    incFile.write("\n")
    #incFile.write("%s;\n" % self.inProto)
    #incFile.write("%s;\n" % self.ckProto)

    # Write To Function Type Maps File
    funcFile.write("\n\n%{")
    funcFile.write("\n".join(self.inFunc))
    funcFile.write("\n\n")
    funcFile.write("\n".join(self.ckFunc))
    funcFile.write("\n%}")
    funcFile.write("\n")

    # Write To Type Maps File
    outFile.write("\n")
    outFile.write("\n".join(self.inMap))
    outFile.write("\n\n")
    outFile.write("\n".join(self.ckMap))
    outFile.write("\n")
    return


#======================================================
#===  STATIC FUNCTIONS TO RETRIEVE FILE NAMES   =======
#======================================================

baseFileName = "VisusSmartPointer%s"

def createFile(filename):
    fh = open(filename, "w")
    fh.write("/* ---------- This file is auto-generated by setup.py  ------------- */\n")
    fh.write("/* ---------- settings to change are in CMakeLists.txt ------------- */\n")
    return fh

def includeFile():
    return createFile(includeFileName)

def outputFile():
    return createFile(outputFileName)

def functionFile():
    return createFile(functionFileName)


