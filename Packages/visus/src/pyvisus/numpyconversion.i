/***********************************************************************
*
* Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
* Produced at the Lawrence Livermore National Laboratory  
* Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
* LLNL-CODE-406031.  
* All rights reserved.  
*   
* This file is part of "Simple and Flexible Scene Graph Version 2.0."
* Please also read BSD_ADDITIONAL.txt.
*   
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are
* met:
*   
* @ Redistributions of source code must retain the above copyright
*   notice, this list of conditions and the disclaimer below.
* @ Redistributions in binary form must reproduce the above copyright
*   notice, this list of conditions and the disclaimer (as noted below) in
*   the documentation and/or other materials provided with the
*   distribution.
* @ Neither the name of the LLNS/LLNL nor the names of its contributors
*   may be used to endorse or promote products derived from this software
*   without specific prior written permission.
*   
*  
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
* A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
* LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
* PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
* LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
* NEGLIGENCE OR OTHERWISE) ARISING
*
***********************************************************************/


%module numpyconversion

%{
#include "numpyconversion.h"
%}

%pythoncode %{

class VisusIncoreEncoder:
  def __init__(self, xdim, ydim, zdim, unit=0):
    self.xdim = xdim
    self.ydim = ydim
    self.zdim = zdim
    self.tdim = 1 
    self.unit = unit
    self.left = []
    self.right= []
    self.fields=[]
    return

  def domain(self, left=[], right=[]):
    if len(left) != 3:
       raise RuntimeError("Left lower corner of bounding box does not have 3 values") 
    if len(right) != 3:
       raise RuntimeError("Right upper corner of bounding box does not have 3 values") 
    self.left = left
    self.right = right
    return

  def field (self, data, name="Unknown"):
    if data is None:
       raise RuntimeError("Given data is None. Can not encode as field")
    fieldEncode = encodeArray(data,name,self.xdim,self.ydim,self.zdim,self.tdim)
    self.fields.append(fieldEncode)
    return

  def __str__(self):
    final = "Incore: %d %d %d %d %d" % (self.xdim,self.ydim,self.zdim,self.tdim,self.unit)
    if len(self.left) == 3 and len(self.right) == 3:
       final += " domain %f %f %f %f %f %f" % (self.left[0],self.left[1],self.left[2],self.right[0],self.right[1],self.right[2])

    for field in self.fields:
       final += " { %s }" % field
    return final

%}

%include numpyconversion.h


