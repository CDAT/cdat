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
#include "VisusReadWriteLock.h"
#include "VisusDataSource.h"
#include "VisusSwigSupport.h"
%}

%import "VisusSTLInterface.i"
%import "VisusBlockData.i"


%ignore VisusDataSource::accessAlignedData(const std::vector<double>& left_lower,
                                           const std::vector<double>& right_upper,
                                           const std::vector<int>& start_strides,
                                           const std::vector<int>& end_strides,
                                           const std::vector<int>& strides,
                                           const int field_index,
                                           VisusBlockData& data,
                                           VisusReadWriteLock& access_lock);

%ignore VisusDataSource::accessSample(const std::vector<double>& position,
                                      int field_index, std::vector<int>& index, 
                                      void* result);

%apply std::vector<double>& IGNORE_INPUT_DOUBLE_VECTOR 
{
  std::vector<double>& left_lower, 
  std::vector<double>& right_upper
};


%apply std::vector<double>& CONVERT_OUTPUT_DOUBLE_VECTOR 
{
  std::vector<double>& left_lower,
  std::vector<double>& right_upper
};


%apply VisusBlockData& IGNORE_INPUT_BLOCKDATA
{
  VisusBlockData& data
}

%apply VisusBlockData& CONVERT_OUTPUT_BLOCKDATA_REPLACE
{
  VisusBlockData& data
}


// The newobject directive will indicate to python that the return
// value is a newly allocated numpy array of which it should take
// ownership
//
// I think this is unnecessary since we use the PyArray_FromDims()
// function to create the numpy array. That should result in the
// correct reference counting.
/*
%newobject VisusDataSource::accessAlignedData(const std::vector<double>& left_lower,
                                              const std::vector<double>& right_upper,
                                              const std::vector<int>& strides,
                                              const int field_index,
                                              VisusBlockData& data);
*/

%extend VisusDataSource {
   /*  SWIG Does Not Auto-Wrap Pure-Virtual Functions */ 

   /*  This will call the correct function since self is of type
       VisusDataSource and boundingBox is a virtual function  */

   /*  Implication of not having this is that VisusDataSource
       types returned from a called wrapped SWIG function will
       otherwise not have this function available             */
   void boundingBox(std::vector<int>& left, std::vector<int>& right)
   {
      self->boundingBox(left,right);
   }
   void domainBoundingBox(std::vector<double>& left, std::vector<double>& right)
   {
      self->domainBoundingBox(left,right);
   }
}

%include "VisusDataSource.h"



