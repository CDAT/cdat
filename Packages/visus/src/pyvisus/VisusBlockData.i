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


%{
#include <vector>
#include "VisusBlockData.h"
#include "VisusSwigSupport.h"
%}

%import "VisusMetricData.i"

// The input typemap indicating that the data buffer is always used as output
%typemap(in, numinputs=0) VisusBlockData& IGNORE_INPUT_BLOCKDATA (VisusBlockData buffer)
{
  $1 = &buffer; // swig maps the VisusBlockData reference to a pointer 
}


// The output typemap converting $1 which is the reference to the
// temporary buffer object to a (newly-allocated) numpy array the
// REPLACE version of this typemap will replace the current result (if
// there is one) with the numpy array
%typemap(argout) VisusBlockData& CONVERT_OUTPUT_BLOCKDATA_REPLACE
{
  $result = convertBlockDataNumpy($1);
}

// The output typemap converting $1 which is the reference to the
// temporary buffer object to a (newly-allocated) numpy array and
// attaching it to the current $result 
%typemap(argout) VisusBlockData& CONVERT_OUTPUT_BLOCKDATA
{
  PyObject* array = convertBlockDataNumpy($1);

  attachResult(&($result),array);
}


%extend VisusBlockData
{
  double __getitem__(const int index)
  {
     switch(self->dataType())
     {
     case PV_FLOAT:
     case PV_FLOAT32:
       {
         const float* data = reinterpret_cast<const float*>(self->data());
         return data[index];
       }
     case PV_FLOAT64:
       {
         const double* data = reinterpret_cast<const double*>(self->data());
         return data[index];
       }
     }
     fprintf(stderr,"warning: __getitem__ not implemented for data type (%d)\n", self->dataType());
     return -1;
  }

  void __setitem__(const int index, double value)
  {
     switch(self->dataType())
     {
     case PV_FLOAT:
     case PV_FLOAT32:
       {
         float* data = reinterpret_cast<float*>(self->data());
         data[index] = value;
       }
       break;
     case PV_FLOAT64:
       {
         double* data = reinterpret_cast<double*>(self->data());
         data[index] = value;
       }
       break;
     default:
       fprintf(stderr,"warning: __setitem__ not implemented for data type (%d)\n", self->dataType());
     }
  }   
}


%include "VisusBlockData.h"


