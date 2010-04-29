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


#include "VisusDataSource.h"


VisusDataSource::VisusDataSource() : mUnit(VISUS_INDEX_SPACE), mLeftLower(3,1), mRightUpper(3,0), mCellSize(3,0)
{
}


void VisusDataSource::domainBoundingBox(std::vector<double>& left_lower, std::vector<double>& right_upper) const
{
  left_lower = mLeftLower;
  right_upper = mRightUpper;
}

void VisusDataSource::domainBoundingBox(VisusBoundingBox& bbox) const
{
  for (int i=0;i<3;i++) {
    bbox[i]   = mLeftLower[i];
    bbox[3+i] = mRightUpper[i];
  }
}


int VisusDataSource::accessAlignedData(const std::vector<double>& left_lower,
                                       const std::vector<double>& right_upper,
                                       const std::vector<int>& strides,
                                       const int field_index,
                                       const VisusTime& current_time,
                                       VisusBlockData& data)
{
  VisusReadWriteLock dummy_lock;

  std::vector<int> start_strides,end_strides;

  start_strides = end_strides = strides;

  return accessAlignedData(left_lower,right_upper,start_strides,end_strides,strides,
                           field_index,current_time,data,dummy_lock);
  return 1;
}


int VisusDataSource::mapDomainToIndexSpace(const std::vector<double>& left_lower, 
                                           const std::vector<double>& right_upper,
                                           std::vector<double>& min_index, 
                                           std::vector<double>& max_index,
                                           std::vector<double>& zoom)
{
  std::vector<int> left(3),right(3); // bounding box in index space
  double p_trans[3]; // The translation of the physical bbox to center
  double i_trans[3]; // The translation of the center to the index bbox
  int i;

  /*
  fprintf(stderr,"Request: [%f, %f, %f] x [%f, %f, %f]\n",left_lower[0],left_lower[1],left_lower[2],
          right_upper[0],right_upper[1],right_upper[2]);
  fprintf(stderr,"\t [%f, %f, %f] x [%f, %f, %f]\n",mLeftLower[0],mLeftLower[1],mLeftLower[2],
          mRightUpper[0],mRightUpper[1],mRightUpper[2]);
  */

  boundingBox(left,right);

  for (i=0;i<3;i++) {
    p_trans[i] = -(mLeftLower[i] + mRightUpper[i]) / 2;
    i_trans[i] = (right[i] - left[i])/2.0;

    // If the i'th coordinate of the samples space is degenerate
    if (left[i] == right[i]) {
      // We cannot map any bounding box and return a 1 as
      // default. Note, that if mRightUpper[i] == mLeftLower[i] this
      // is the correct thing to do. If mRightUpper[i] !=
      // mLeftLower[i] then the clamping to the index space later in
      // the pipeline should take care of the problem

      zoom[i] = 1;
    }
    else {
      zoom[i] = (right[i] - left[i]) / (mRightUpper[i] - mLeftLower[i]);
    }
  }
        
  //fprintf(stderr,"zoom: %f %f %f\n",zoom[0],zoom[1],zoom[2]);

  for (i=0;i<3;i++) {
    min_index[i] = zoom[i]*(left_lower[i] + p_trans[i]) + i_trans[i];
    max_index[i] = zoom[i]*(right_upper[i] + p_trans[i]) + i_trans[i];
  }
  
  /*
  fprintf(stderr,"\t [%f, %f, %f] x  [%f, %f, %f]\n", min_index[0],min_index[1],min_index[2],
          max_index[0],max_index[1],max_index[2]);
  */
  return 1;
}

//int VisusDataSource::mapDomainToIndexSpace(const std::vector<double>& sample, 
//                                           std::vector<double>& min_index)


