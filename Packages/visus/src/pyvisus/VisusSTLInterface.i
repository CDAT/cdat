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


%include "std_vector.i"
%include "std_string.i"
%include "VisusSwigSupport.h"

namespace std {
  %template(VectorInt) vector<int>;
  %template(VectorDouble) vector<double>;
  %template(VectorFloat) vector<float>;
  %template(VectorPUChar) vector<unsigned char*>;
}


%typemap(in, numinputs=0) std::vector<double>& IGNORE_INPUT_DOUBLE_VECTOR (std::vector<double> temp)
{
  $1 = &temp;
}

%typemap(in, numinputs=0) std::vector<float>& IGNORE_INPUT_FLOAT_VECTOR (std::vector<float> temp)
{
  $1 = &temp;
}

%typemap(in, numinputs=0) std::vector<int>& IGNORE_INPUT_INT_VECTOR (std::vector<int> temp)
{
  $1 = &temp;
}

%typemap(in, numinputs=0) std::vector<unsigned char*>& IGNORE_INPUT_PUCHAR_VECTOR (std::vector<unsigned char*> temp)
{
  $1 = &temp;
}


%typemap(argout) std::vector<double>& CONVERT_OUTPUT_DOUBLE_VECTOR
{
  int dim;

  dim = $1->size();

  // Allocate the new array
  PyArrayObject* array = (PyArrayObject*)PyArray_FromDims(1,&dim,PyArray_DOUBLE);
  
  for (int i=0;i<dim;i++) {
    ((double*)(array->data))[i] = (*$1)[i];
  }

  attachResult(&($result),PyArray_Return(array));
}

%typemap(argout) std::vector<float>& CONVERT_OUTPUT_FLOAT_VECTOR
{
  int dim;

  dim = $1->size();

  // Allocate the new array
  PyArrayObject* array = (PyArrayObject*)PyArray_FromDims(1,&dim,PyArray_FLOAT);
  
  for (int i=0;i<dim;i++) {
    ((float*)(array->data))[i] = (*$1)[i];
  }

  attachResult(&($result),PyArray_Return(array));
}

%typemap(argout) std::vector<int>& CONVERT_OUTPUT_INT_VECTOR
{
  int dim;

  dim = $1->size();

  // Allocate the new array
  PyArrayObject* array = (PyArrayObject*)PyArray_FromDims(1,&dim,PyArray_INT);
  
  for (int i=0;i<dim;i++) {
    ((int*)(array->data))[i] = (*$1)[i];
  }
  
  attachResult(&($result),PyArray_Return(array));
}

%typemap(argout) std::vector<unsigned char*>& CONVERT_OUTPUT_PUCHAR_VECTOR
{
  int dim;

  dim = $1->size();

  // Allocate the new array
  PyArrayObject* array = (PyArrayObject*)PyArray_FromDims(1,&dim,PyArray_INT);
  
  for (int i=0;i<dim;i++) {
    ((int*)(array->data))[i] = (*$1)[i];
  }
  
  attachResult(&($result),PyArray_Return(array));
}

