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


#include <cstdio>
#include <cstdlib>
#include <string>
#include <sstream>

#include "VisusMath.h"

#include <float.h>
#include <string.h>
#include <assert.h>

#include "VisusIncoreDataSource.h"
#include "VisusStdInt.h"
#include "VisusDefinitions.h"

using namespace std;

VisusIncoreDataSource::VisusIncoreDataSource(const std::string& args) : 
  VisusDataSource()
{
  std::vector<std::string> tokens;

  tokens = tokenize(args);
  
  if (initializeFromString(tokens) == 0) {
    
    vwarning("Data source string inconsistent. Could not intialzize incore data source.");
    
    mSamples = std::vector<int>(3,0);
  }
}

//! Destructor
VisusIncoreDataSource::~VisusIncoreDataSource() 
{
}

bool VisusIncoreDataSource::isValid() const
{
  if (!mData.empty() && (mData[0] != NULL))
    return true;
  
  return false;
}


void VisusIncoreDataSource::boundingBox(std::vector<int>& left_lower, std::vector<int>& right_upper) const
{
  left_lower[0] = left_lower[1] = left_lower[2] = 0;
  right_upper[0] = mSamples[0]-1;
  right_upper[1] = mSamples[1]-1;
  right_upper[2] = mSamples[2]-1;
}

PvDataType VisusIncoreDataSource::fieldType(int i) const 
{
  if ((i < 0) || (i >= (int)mData.size())) {
    vwarning("Index out of range no such field.");
    return PV_RAW;
  }

  return mDataType[i];
}

int VisusIncoreDataSource::fieldSampleSize(int i) const 
{
  if ((i < 0) || (i >= (int)mData.size())) {
    vwarning("Index out of range no such field.");
    return 1;
  }

  return mDataSize[i];
}

void VisusIncoreDataSource::fieldRange(int i, double& min_f, double& max_f) const
{
  if ((i < 0) || (i >= (int)mData.size())) {
    vwarning("Index out of range no such field.");
    return;
  }

  min_f = mFieldRange[i].r[0];
  max_f = mFieldRange[i].r[1];
}

std::string VisusIncoreDataSource::fieldName(int i) const
{
  if ((i < 0) || (i >= (int)mData.size())) {
    vwarning("Index out of range no such field.");
    return "NoName";
  }
  
  return mFieldName[i];
}

int VisusIncoreDataSource::accessAlignedData(const std::vector<double>& left_lower,
                                             const std::vector<double>& right_upper,
                                             const std::vector<int>& start_strides,
                                             const std::vector<int>& end_strides,
                                             const std::vector<int>& strides,
                                             const int field_index,
                                             const VisusTime& current_time,
                                             VisusBlockData& data,
                                             VisusReadWriteLock& access_lock)
{
  std::vector<double> start(3),stop(3); // The requested bounding box in index space
  std::vector<int> left(3),right(3); // The requested box clamped to valid coordinates
  std::vector<double> adjust(3); // Adjustment between the requested bbox and the actual box returned
  std::vector<double> extent(3); // The extent of the valid bounding box in index space
  std::vector<int> samples(3);  // The number of samples in each dimension
  std::vector<double> scale(3); // scale factors to map from index to physical space
  std::vector<double> zoom(3); // scale factor to go from node centered to cell centered data
  VisusTransformation3D matrix,cell_centered;

  if (field_index >= numberOfFields()) {
    vwarning("Index out of range no such field.");
    return 0;
  }

  //this->mTimeInfo.time(current_time);

  // Ensure valid current time parameter
  const int time_offset = timeOffset(current_time);
  if (time_offset < 0)
    return 0;


  // Figure out the requested bounding box in index space and the
  // scale factor between index and physical space
  if (mapDomainToIndexSpace(left_lower,right_upper,start,stop,scale) == 0) {
    vwarning("Incorrect domain mapping, aborting extraction.");
    return 0;
  }

  // If the requested bounding box is completey outside the data set 
  if ((start[0] > mSamples[0]-1) || (start[1] > mSamples[1]-1) || (start[2] > mSamples[2]-1)
      || (stop[0] < 0) || (stop[1] < 0) || (stop[2] < 0)) {
    
    // use some default values
    samples[0] = samples[1] = samples[2] = 1;
    extent[0] = extent[1] = extent[2] = 0;
  }
  else {
    // Otherwise, map the request to the correct resolution
    
    for (int i=0;i<3;i++) {
      left[i] = MAX(0,strides[i]*round(start[i] / strides[i]));
      right[i] = MIN(mSamples[i]-1,strides[i]*round(stop[i] / strides[i]));
      
      extent[i] = right[i] - left[i];
      samples[i] = extent[i] / strides[i] + 1;
      
      adjust[i] = start[i] - left[i];
    
      if (left[i] == right[i])
        zoom[i] = 1;
      else
        //zoom[i] = (extent[i] + strides[i]*mCellSize[i]) / extent[i];
        zoom[i] = (extent[i] + 1) / extent[i];

    }
    
    // Now we must determine the transformation matrix for this piece
    // of data. The boundary conditions are that when the data is
    // drawn it will be drawn in index space modulated by the
    // transformation matrix. As a result the data should be drawn
    // with the left lower corner at the left lower corner of the
    // physical space. As an additional complication left[i] !=
    // start[i] which means that there was not enough data for all of
    // the query. As always remember that matrixes are applied in
    // reverse order.
    //
    // Step 1: translate to adjust for cutting off data on the low side
    // Step 2: translate the center of the extent box to (0,0,0)
    // Step 3: scale the index space coordinates to physical space
    // Step 4: translate the lower left corner of the physical bbox to (0,0,0)
    
    
    
    // Step 4
    matrix = translationMatrix((right_upper[0] - left_lower[0])/2,
                               (right_upper[1] - left_lower[1])/2,
                               (right_upper[2] - left_lower[2])/2);
    
    // Step 3
    matrix *= scaleMatrix(1/scale[0],1/scale[1],1/scale[2]);
    
    
    // Step 2 + 1
    matrix *= translationMatrix(-(stop[0] - start[0])/2 - adjust[0],
                                -(stop[1] - start[1])/2 - adjust[1],
                                -(stop[2] - start[2])/2 - adjust[2]);

    /*
    fprintf(stderr,"Data matrix:\n %f %f %f %f\n %f %f %f %f\n %f %f %f %f\n",
            matrix[0],matrix[4],matrix[8],matrix[12],
            matrix[1],matrix[5],matrix[9],matrix[13],
            matrix[2],matrix[6],matrix[10],matrix[14]);
    */      
    
    // Furthemore, we need to compute the cell_centered matrix that
    // adjusts the physical coordinates to display in cell-centered
    // mode
    //
    // Step 1: translate the center of the actual data to (0,0,0)
    // Step 2: scale the data according to the resolution and cell sizes
    // Step 3: revert the translation

    // Step 3
    cell_centered = translationMatrix(extent[0]/2,extent[1]/2,extent[2]/2);
    
    // Step 2
    cell_centered *= scaleMatrix(zoom[0],zoom[1],zoom[2]);
    
    // Step 1
    cell_centered *= translationMatrix(-extent[0]/2,-extent[1]/2,-extent[2]/2);

    /*
    fprintf(stderr,"VisusIncoreDataSource::accessAlignedData:\n\t [%f,%f,%f] x [%f,%f,%f] at strides <%d,%d,%d>\n\
      \t extracted as [%d,%d,%d] x [%d,%d,%d] with [%d,%d,%d] samples\n \
      \t cellCentered adjustment <%f, %f, %f>\n                         \
      \t current time step %f    offset %d\n \ 
      \t current pointer %d\n\n",
            start[0],start[1],start[2],stop[0],stop[1],stop[2],
            strides[0],strides[1],strides[2],
            left[0],left[1],left[2],right[0],right[1],right[2],samples[0],samples[1],samples[2],
            zoom[0],zoom[1],zoom[2],current_time.toDouble(),time_offset,mData[field_index]);
    */
    
  }


  // First we must obtain a write lock
  if (access_lock.writeLock() == 0) {
    vwarning("Could not obtain write lock to load new data. New data ignored.");
    return 0;
  }


  // Set the VisusData specific components
  //data.matrix(translationMatrix(adjust[0],adjust[1],adjust[2]));
  data.matrix(matrix);
  data.cellCentered(cell_centered);
  data.incId();

  // Set the VisusBlockData specific components
  data.dataType(mDataType[field_index]);
  data.sampleSize(mDataSize[field_index]);
  data.extent(extent);
  data.samples(samples);

  // Set the field range
  data.setFieldRange(mFieldRange[field_index].r[0],mFieldRange[field_index].r[1]);

  // Make sure we have enough space for the data we are about to copy
  data.reserveSpace();

  // Make sure to access the current time
  vverbose("VisusIncoreDataSource - Time offset for data %d\n", VISUS_TIME_VERBOSE, time_offset);

  // Copy the data 
  switch (mDataType[field_index]) {

  case PV_RAW: 
    break;
  case PV_UCHAR:
    
    copySamples<unsigned char>(left,right,strides,samples,time_offset,field_index,data);
    break;
  case PV_FLOAT: 
  case PV_FLOAT32:
    {
      float* buffer = (float*)mData[field_index];
      float* target = (float*)data.data();
      int i,j,k;
      
      for (i=left[2];i<=right[2];i+=strides[2]) {
        for (j=left[1];j<=right[1];j+=strides[1]) {
          for (k=left[0];k<=right[0];k+=strides[0]) {
            *target = *(buffer + k +j*mSamples[0] + i*mSamples[1]*mSamples[0] + time_offset);
            target++;
           }
        }
      }
      // The mask is now set below I have no idea what his code was supposed to accomplish
      // (ptb) 02/13/09
      //if ((i == left[2]) && (j == left[1]) && (k == left[0])) 
      //  data.mask(0,false);
      
      break;
    }

  case PV_FLOAT64:
    {
      double* buffer = (double*)mData[field_index];
      double* target = (double*)data.data();
      int i,j,k;
      
      for (i=left[2];i<=right[2];i+=strides[2]) {
        for (j=left[1];j<=right[1];j+=strides[1]) {
          for (k=left[0];k<=right[0];k+=strides[0]) {
            *target = *(buffer + k +j*mSamples[0] + i*mSamples[1]*mSamples[0] + time_offset);
            target++;
          }
        }
      }
      
      // The mask is now set below I have no idea what his code was supposed to accomplish
      // (ptb) 02/13/09
      //if ((i == left[2]) && (j == left[1]) && (k == left[0])) 
      //  data.mask(0,false);

      break;
    }
  default:
    printf("type %d is not implemented\n", mDataType[field_index]);
    assert (false); 
    break;
  }

  
  if (mMask[field_index] != NULL) {
    uint8_t* mask = (uint8_t*)mMask[field_index];
    int i,j,k;
    int count = 0;

    for (i=left[2];i<=right[2];i+=strides[2]) {
      for (j=left[1];j<=right[1];j+=strides[1]) {
        for (k=left[0];k<=right[0];k+=strides[0]) {
          data.mask(count,mask[k +j*mSamples[0] + i*mSamples[1]*mSamples[0]]);
          ++count;
        }
      }
    }
  } 
           
    

  //fprintf(stderr,"Done with extracting data\n");

  return 1;
}

int VisusIncoreDataSource::accessSample(const std::vector<double>& position,
                                        const int field_index,  
                                        const VisusTime& current_time,
                                        std::vector<int>& index, 
                                        void* result)
{
  if (!isValid()) {
    vwarning("Trying to acess invalid data source");
    return 0;
  }

  if (field_index >= numberOfFields()) {
    vwarning("Index out of range no such field.");
    return 0;
  }

  this->mTimeInfo.time(current_time);

  // Ensure valid current time parameter
  const int time_offset = timeOffset(current_time);
  if (time_offset < 0)
    return 0;

  std::vector<double> start(3),stop(3); // The requested bounding box in index space
  std::vector<double> dummy(3);

  mapDomainToIndexSpace(position,position,start,stop,dummy);

  // Compute the indices. Warning: this might be a threading hazard
  index[0] = MIN(mSamples[0]-1,MAX(0,start[0]));
  index[1] = MIN(mSamples[1]-1,MAX(0,start[1]));
  index[2] = MIN(mSamples[2]-1,MAX(0,start[2]));

  const int iOffset = index[0];
  const int jOffset = index[1] * mSamples[0];
  const int kOffset = index[2] * mSamples[0] * mSamples[1];

  vmessage("VisusIncoreDataSource::accessSample: index(%d,%d,%d) offset(%d,%d,%d,%d)\n", VISUS_TIME_VERBOSE,
           index[0],index[1],index[2], iOffset,jOffset,kOffset,time_offset);

  memcpy(result,mData[field_index] + mDataSize[field_index]*(iOffset+jOffset+kOffset+time_offset),
                                   mDataSize[field_index]);
  return 1;
}
                 
  

// Convert a value to a double.
static double toDouble(const unsigned char* value, PvDataType dtype)
{
  double v = 0.0;
  switch (dtype) {
  case PV_RAW:   //First the types we cannot handle
  case PV_INT64:
  case PV_UINT64:
  case PV_RGB:
  case PV_RGBA:
    break;
  case PV_CHAR:
    v = (double) *(  (int8_t *)value);
    break;
  case PV_UCHAR:
    v = (double) *(  (uint8_t *)value);
    break;
  case PV_INT:
    v = (double) *(  (int *)value);
    break;
  case PV_INT16:
    v = (double) *(  (int16_t *)value);
    break;
  case PV_UINT16:
    v = (double) *(  (uint16_t *)value);
    break;
  case PV_INT32:
    v = (double) *(  (int32_t *)value);
    break;
  case PV_UINT32:
    v = (double) *(  (uint32_t *)value);
    break;
  case PV_FLOAT:
    v = (double) *(  (float *)value);
    break;
  case PV_FLOAT32:
    v = (double) *(  (float *)value);
    break;
  case PV_FLOAT64:
    v = *(  (double *)value);
    break;
  }
  return v;
}

void VisusIncoreDataSource::computeMinMaxBound(unsigned char* data, PvDataType data_type,
                                               int data_size,double& min_val, double& max_val)
{
  double val = toDouble(data,data_type);
  
  min_val = max_val = val;

  for (int i=0;i<mSamples[0]*mSamples[1]*mSamples[2];i++) {
    
    val = toDouble(data + i*data_size,data_type);
    min_val = min(min_val,val);
    max_val = max(max_val,val);
  }
}

std::vector<std::string> VisusIncoreDataSource::tokenize(const std::string& str) const 
{
  vector<string> tokens;
  std::string delimiters = " ";

  // skip delimiters at beginning.
  string::size_type lastPos = str.find_first_not_of(delimiters, 0);
  
  // find first "non-delimiter".
  string::size_type pos = str.find_first_of(delimiters, lastPos);
  
  while (string::npos != pos || string::npos != lastPos) {
    // found a token, add it to the vector.
    tokens.push_back(str.substr(lastPos, pos - lastPos));
    
    // skip delimiters.  Note the "not_of"
    lastPos = str.find_first_not_of(delimiters, pos);
    
    // find next "non-delimiter"
    pos = str.find_first_of(delimiters, lastPos);
  }

  //for (vector<string>::iterator it=tokens.begin();it!=tokens.end();it++)
  //fprintf(stderr,"Token %s\n",it->c_str());
  
  return tokens;
  
}

int VisusIncoreDataSource::initializeFromString(std::vector<std::string>& tokens)
{
  if (tokens.size() < 5) {
    vwarning("Incorrect number of tokens to initialize incore datasource.");
    return 0;
  }

  char *ep;

  mSamples.resize(3);
  mSamples[0] = strtol(tokens[0].c_str(),&ep,10);
  mSamples[1] = strtol(tokens[1].c_str(),&ep,10);
  mSamples[2] = strtol(tokens[2].c_str(),&ep,10);

  // Set time parameters
  vverbose("VisusIncoreDataSource - Parse time dimension\n", VISUS_TIME_VERBOSE);
  int timeLen = strtol(tokens[3].c_str(),&ep,10);
  this->mTimeInfo = VisusGlobalTime();
  this->mTimeInfo.end(VisusIntTime(timeLen-1));
  this->mTimeInfo.step(VisusIntTime(1));
  vverbose("VisusIncoreDataSource - Time index range [%f, %f]\n", VISUS_TIME_VERBOSE, 
           this->mTimeBegin.toDouble(), this->mTimeEnd.toDouble());

  mUnit = convert_to_unit(strtol(tokens[4].c_str(),&ep,10));

  std::vector<std::string>::iterator it;

  
  it = tokens.begin()+5;
  while (it != tokens.end()) {
       
    if (*it == "domain") {
      it++;
      mLeftLower[0] = strtod(it->c_str(),&ep);
      it++;
      mLeftLower[1] = strtod(it->c_str(),&ep);
      it++;
      mLeftLower[2] = strtod(it->c_str(),&ep);
      it++;
      mRightUpper[0] = strtod(it->c_str(),&ep);
      it++;
      mRightUpper[1] = strtod(it->c_str(),&ep);
      it++;
      mRightUpper[2] = strtod(it->c_str(),&ep);
    }
    else if (*it == "time") {
      it++;
      this->mTimeInfo.initializeFromString(it,timeLen);
    }
    else if (*it == "{") {
      if (parseFieldData(it,tokens) == 0) {
        vwarning("Could not parse field data.");
        return 0;
      }
    }
    else {
      vwarning("Unrecognized token when initializing incore data source.");
      return 0;
    }
    
    it++;
  }

  // Check whether we have read a valid domain bounding box and we are
  // not in index space. If not the unit should be index space and we
  // use the mSamples as box
  if ((mUnit == VISUS_INDEX_SPACE) || (mLeftLower[0] > mRightUpper[0]) || (mLeftLower[1] > mRightUpper[1])
      || (mLeftLower[2] > mRightUpper[2])) {
    
    if (mUnit != VISUS_INDEX_SPACE) {
      vwarning("Datasource unit not index space but invalid domain chaning usints to index space.");
      mUnit = VISUS_INDEX_SPACE;
    }
    
    mLeftLower[0] = mLeftLower[1] = mLeftLower[2] = 0;
    mRightUpper[0] = mSamples[0]-1;
    mRightUpper[1] = mSamples[1]-1;
    mRightUpper[2] = mSamples[2]-1;
  }

  // Now that we have determined the physical bounding box we can
  // calculate the width of half a grid cell at the finest
  // resolution. For this purpose we think of the grid as cell
  // centered data where the *center* of the first cell lies at
  // mLeftLower and the *center* of the the last cell lies at
  // mRightUpper. This will be used to adjust for the cell centered
  // drawing style of textures / volume renderings
  for (int i=0;i<3;i++) {
    mCellSize[i] = (mRightUpper[i] - mLeftLower[i]) / (mSamples[i] - 1);
  }

  return 1;
}

int VisusIncoreDataSource::parseFieldData(std::vector<std::string>::iterator& it,
                                          const std::vector<std::string>& tokens)
{
  if (tokens.end() - it < 6) {
    vwarning("Too few tokens for valid field dscription.");
    return 0;
  }

  std::string name;
  unsigned char* pointer;
  PvDataType type;
  int data_size;
  unsigned char* mask = NULL;
  FieldRange range;
  
  if (*it != "{") {
    vwarning("Unrecognized token when parsing field data.");
    return 0;
  }

  it++;
  name = *it;
  
  it++;
  pointer = (unsigned char*)atoi(it->c_str());
  

  it++;
  type = (PvDataType)atoi(it->c_str());

  it++;
  data_size = atoi(it->c_str());

  // Initialize the range with illegal values to determine whether we
  // found a range
  range.r[0] = 1;
  range.r[1] = -1;

  it++;
  while ((it != tokens.end()) && (*it != "}")) {
    
    //fprintf(stderr,"VisusIncoreDataSource::  parsing token \"%s\"\n",it->c_str());
    
    if (*it == "range") {
      it++;
      range.r[0] = atof(it->c_str());
      it++;
      range.r[1] = atof(it->c_str());
    }
    else if (*it == "mask") {
      it++;
      mask = (unsigned char*)atoi(it->c_str());
    }
    else {
      fprintf(stderr,"token %s\n",it->c_str());
      vwarning("Undefined token when parsing field data."); 
    }

    it++;
  }

  if (*it != "}") {
    vwarning("Field data inconsistent aborting input.");
    return 0;
  }

  // Check whether we have read a valid range. If not compute the
  // range
  if (range.r[0] > range.r[1]) 
    computeMinMaxBound(pointer,type,data_size,range.r[0],range.r[1]);
    
  // Now that we have collected a consistent set of field information
  // we store it.

  mData.push_back(pointer);
  mDataType.push_back(type);
  mDataSize.push_back(data_size);
  mMask.push_back(mask);
  mFieldName.push_back(name);
  mFieldRange.push_back(range);

  return 1;
}

int VisusIncoreDataSource::timeOffset(const VisusTime& current_time)
{
  int offset;
  
  this->mTimeInfo.time(current_time);
  offset = this->mTimeInfo.timeIndex();

  offset *= mSamples[0]*mSamples[1]*mSamples[2];

  return offset;
}
