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


#include "VisusFastLoopDataSource.h"
#include "VisusDefinitions.h"

#ifndef VISUS_DISABLE_IDX
#include "datatype.h"

const double VisusFastLoopDataSource::sHugeValue = 10e43;

VisusFastLoopDataSource::VisusFastLoopDataSource(const std::string& datainfo) : VisusDataSource()
{
  if (mInterface.load_hzfile(datainfo.c_str()))
    mValid = true;
  else
    mValid = false;

  if (mValid) {
    mReadBuffer = new data_result;
    mWriteBuffer = new data_result;

    // For now we assume idx data is always given in index space
    this->mUnit = VISUS_INDEX_SPACE;

    this->mLeftLower[0] =  mInterface.hzfile().boundingbox[0];
    this->mLeftLower[1] =  mInterface.hzfile().boundingbox[2];
    this->mLeftLower[2] =  mInterface.hzfile().boundingbox[4];

    this->mRightUpper[0] =  mInterface.hzfile().boundingbox[1];
    this->mRightUpper[1] =  mInterface.hzfile().boundingbox[3];
    this->mRightUpper[2] =  mInterface.hzfile().boundingbox[5];

    // Since we are using index space the cell size is trivially 1
    this->mCellSize[0] = 1;
    this->mCellSize[1] = 1;
    this->mCellSize[2] = 1;
  }
    
}

VisusFastLoopDataSource::~VisusFastLoopDataSource()
{
  mInterface.clear();

  delete mReadBuffer;
  delete mWriteBuffer;
}

int VisusFastLoopDataSource::interrupt()
{
  if (!mValid) {
    vwarning("Cannot interrupt invalid data source.");
    return 0;
  }

  mInterface.interrupt_access(true);
  
  return 1;
}

std::vector<int> VisusFastLoopDataSource::samples() const
{
  std::vector<int> dim(3,1);

  if (mValid) {
    dim[0] = mInterface.hzfile().boundingbox[1] - mInterface.hzfile().boundingbox[0] + 1;
    dim[1] = mInterface.hzfile().boundingbox[3] - mInterface.hzfile().boundingbox[2] + 1;
    dim[2] = mInterface.hzfile().boundingbox[5] - mInterface.hzfile().boundingbox[4] + 1;
  }

  return dim;
}

int VisusFastLoopDataSource::dimensions() const
{
  return 3;
}


void VisusFastLoopDataSource::boundingBox(std::vector<int>& left_lower, std::vector<int>& right_upper) const
{
  if (mValid) {
    left_lower.resize(3);
    right_upper.resize(3);

    left_lower[0]  = mInterface.hzfile().boundingbox[0];
    right_upper[0] = mInterface.hzfile().boundingbox[1];
    left_lower[1]  = mInterface.hzfile().boundingbox[2];
    right_upper[1] = mInterface.hzfile().boundingbox[3];
    left_lower[2]  = mInterface.hzfile().boundingbox[4];
    right_upper[2] = mInterface.hzfile().boundingbox[5];
  } 
}

PvDataType VisusFastLoopDataSource::fieldType(int i) const
{
  if (!mValid)
    return PV_RAW;

  switch (mInterface.hzfile().bytespersample) {
  case 1:
    return PV_UCHAR;
  case 2:
    return PV_UINT16;
  case 3:
    return PV_RGB;
  case 4:
    return PV_FLOAT32;
  case 8:
    return PV_FLOAT64;
  default:
    return PV_RAW;
  }
}

int VisusFastLoopDataSource::numberOfFields() const 
{
  return 1;
}

int VisusFastLoopDataSource::fieldSampleSize(int i) const
{
  if ((i < 0) || (i >= numberOfFields())) {
    vwarning("No such field index out of range.");
    return 1;
  }

  return mInterface.hzfile().bytespersample;
}

void VisusFastLoopDataSource::fieldRange(int i, double& min_f, double& max_f) const
{
  vwarning("Min/Max field range not currently stored. Returning defaults.");
  min_f = 0;
  max_f = 1;
}

std::string VisusFastLoopDataSource::fieldName(int i) const
{
  vwarning("Field name not currently stored. Returning default.");
  return "density";
}

int VisusFastLoopDataSource::accessAlignedData(const std::vector<double>& left_lower,
                                               const std::vector<double>& right_upper,
                                               const std::vector<int>& start_strides,
                                               const std::vector<int>& end_strides,
                                               const std::vector<int>& strides,
                                               const int field_index,
                                               const VisusTime& current_time,
                                               VisusBlockData& data,
                                               VisusReadWriteLock& access_lock)
{
  data_params param;
  std::vector<double> start(3),stop(3); // The requested bounding box in index space
  std::vector<double> adjust(3); // Adjustment between the requested bbox and the actual box returned
  std::vector<double> extent(3); // The extent of the valid bounding box in index space
  std::vector<int> samples(3);  // The number of samples in each dimension
  std::vector<double> scale(3); // scale factors to map from index to physical space
  std::vector<double> zoom(3); // scale factor to go from node centered to cell centered data
  VisusTransformation3D matrix,cell_centered;
  double min_f,max_f;
  int tmp;

  // Figure out the requested bounding box in index space and the 
  // scale factor between index and physical space
  if (mapDomainToIndexSpace(left_lower,right_upper,start,stop,scale) == 0) {
    vwarning("Incorrect domain mapping, aborting extraction.");
    return 0;
  }

  /************************************************************************
   ****************   Data extraction  ************************************
   ************************************************************************/

  param.flags |= BITMASK_FLAG;

  // Setup the internal request structure

  // Convert the strides based input resolution into the bit-pattern
  // used by the fast loop library
  tmp = start_strides[0] >> 1;
  param.start = 0;
  while (tmp > 0) {
    tmp >>= 1;
    ++param.start;
  }

  tmp = end_strides[0] >> 1;
  param.end = 0;
  while (tmp > 0) {
    tmp >>= 1;
    ++param.end;
  }

  // Copy the bounding box information
  param.range[0] = MAX(start[0],0);
  param.range[1] = MAX(stop[0],0);
  
  param.range[2] = MAX(start[1],0);
  param.range[3] = MAX(stop[1],0);

  param.range[4] = MAX(start[2],0);
  param.range[5] = MAX(stop[2],0);

  if (param.invalid()) {
    vwarning("Parameters invalid.");
    return 0;
  }
  
  // If this is the first resolution we extract 
  if (strides == start_strides) {
    // Fill the write buffer ignoring the data in the read buffer
    if (!accessData(param, mWriteBuffer)) {
      return 0;
    }
  }
  else {// Otherwise mReadBuffer contains the previous resolution
    if (strides[0] != (1 << (mReadBuffer->level/3-1))) {
      vwarning("Levels of previous resolution are not consistent.");
    }
    
    if (!updateData(param,mReadBuffer,mWriteBuffer)) {
      return 0;
    }
  }
  
  // "Copy" the new data into mReadBuffer by swapping the pointers
  std::swap(mReadBuffer,mWriteBuffer);

  /************************************************************************
   ****************   Data transformation  ********************************
   ************************************************************************/
  // Compute the data description in the way VisusBlockData needs it

  // Compute the extent of the extracted bounding box
  extent[0] = mReadBuffer->range[1] - mReadBuffer->range[0];
  extent[1] = mReadBuffer->range[3] - mReadBuffer->range[2];
  extent[2] = mReadBuffer->range[5] - mReadBuffer->range[4];

  // Copy the number of samples
  samples[0] = mReadBuffer->dims[0];
  samples[1] = mReadBuffer->dims[1];
  samples[2] = mReadBuffer->dims[2];

  // Compute the adjustment between the requested box and the actual
  // box returned
  adjust[0] = start[0] - mReadBuffer->range[0];
  adjust[1] = start[1] - mReadBuffer->range[2];
  adjust[2] = start[2] - mReadBuffer->range[4];

  // Compute the cell centered scaling factor
  for (int i=0;i<3;i++) {
    if (extent[i] == 0) 
      zoom[i] = 1;
    else 
      //zoom[i] = (extent[i] + strides[i]*mCellSize[i]) / extent[i];
      zoom[i] = (extent[i] + 1) / extent[i];
  }

  
  // Now we must determine the transformation matrix for this piece
  // of data. The boundary conditions are that when the data is
  // drawn it will be drawn in index space modulated by the
  // transformation matrix. As a result the data should be drawn
  // with the left lower corner at 0 in physical space. As an
  // additional complication left[i] != start[i] which means that
  // there was not enough data for all of the query. As always
  // remember that matrixes are applied in reverse order.
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
  matrix *= scaleMatrix(scale[0],scale[1],scale[2]);
  
  
  // Step 2 + 1
  matrix *= translationMatrix(-(stop[0] - start[0])/2 - adjust[0],
                              -(stop[1] - start[1])/2 - adjust[1],
                              -(stop[2] - start[2])/2 - adjust[2]);
  
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
  


  // Since the idx file does not store the range we must comupte it
  // computeMinMaxBounds(mReadBuffer,min_f,max_f);
  min_f = 0;
  max_f = 0.6;
  
  /************************************************************************
   ****************   Data transfer ***************************************
   ************************************************************************/
    
  // At this point mReadBuffer contains the new data which we want to
  // transfer into data
    
  // First we must obtain a write lock
  if (access_lock.writeLock() == 0) {
    vwarning("Could not obtain write lock to load new data. New data ignored.");
    return 0;
  }
  
  
  // Set the VisusData specific components
  data.matrix(matrix);
  data.cellCentered(cell_centered);
  data.incId();

  // Set the VisusBlockData specific components
  data.dataType(fieldType(field_index));
  data.sampleSize(fieldSampleSize(field_index));
  data.extent(extent);
  data.samples(samples);
  data.setFieldRange(min_f,max_f);
  data.dataSize(mReadBuffer->size);
  data.data((unsigned char*)mReadBuffer->buffer);
  data.maskSize(mReadBuffer->msize);
  data.setMask(mReadBuffer->bitmask);

  //fprintf(stderr,"VisusFastLoopDataSource: returned strides %d %d %d\n",strides[0],strides[1],strides[2]);

  /* 
  // The new data source interface says that the data should be locked
  // when returning from this function

  if (access_lock.unlock() == 0) {
    vwarning("Could not unlock data after loading new data.");
    return 0;
  }
  */

  return 1;
}

bool VisusFastLoopDataSource::accessData(data_params& param, data_result* write)
{
  switch (fieldType(0)) {

  case PV_RAW:
    return mInterface.access_samples<char,3>(param, *write);
    break;
  case PV_CHAR:
    return mInterface.access_samples<int8_t,1>(param, *write);
    break;
  case PV_UCHAR:
    return mInterface.access_samples<uint8_t,1>(param, *write);
    break;
  case PV_INT16:
    return mInterface.access_samples<int16_t,1>(param, *write);
    break;
  case PV_UINT16:
    return mInterface.access_samples<uint16_t,1>(param, *write);
    break;
  case PV_INT:
    return mInterface.access_samples<int,1>(param, *write);
    break;
  case PV_INT32:
    return mInterface.access_samples<int32_t,1>(param, *write);
    break;
  case PV_UINT32:
    return mInterface.access_samples<uint32_t,1>(param, *write);
    break;
  case PV_INT64:
    return mInterface.access_samples<int64_t,1>(param, *write);
    break;
  case PV_UINT64:
    return mInterface.access_samples<uint64_t,1>(param, *write);
    break;
  case PV_FLOAT:
    return mInterface.access_samples<float,1>(param, *write);
    break;
  case PV_FLOAT32:
    return mInterface.access_samples<float,1>(param, *write);
    break;
  case PV_FLOAT64:
    return mInterface.access_samples<double,1>(param, *write);
    break;

  case PV_RGB:
    return mInterface.access_samples<char,3>(param, *write);
    break;
  case  PV_RGBA:
    return mInterface.access_samples<char,4>(param, *write);
    break;
    
  }    
    
  return false;
}

bool VisusFastLoopDataSource::updateData(data_params& param,data_result* read, data_result* write)
{
  switch (fieldType(0)) {

  case PV_RAW:
    return mInterface.update_samples<char,3>(param, *read, *write);
    break;
  case PV_CHAR:
    return mInterface.update_samples<int8_t,1>(param, *read, *write);
    break;
  case PV_UCHAR:
    return mInterface.update_samples<uint8_t,1>(param, *read, *write);
    break;
  case PV_INT16:
    return mInterface.update_samples<int16_t,1>(param, *read, *write);
    break;
  case PV_UINT16:
    return mInterface.update_samples<uint16_t,1>(param, *read, *write);
    break;
  case PV_INT:
    return mInterface.update_samples<int,1>(param, *read, *write);
    break;
  case PV_INT32:
    return mInterface.update_samples<int32_t,1>(param, *read, *write);
    break;
  case PV_UINT32:
    return mInterface.update_samples<uint32_t,1>(param, *read, *write);
    break;
  case PV_INT64:
    return mInterface.update_samples<int64_t,1>(param, *read, *write);
    break;
  case PV_UINT64:
    return mInterface.update_samples<uint64_t,1>(param, *read, *write);
    break;
  case PV_FLOAT:
    return mInterface.update_samples<float,1>(param, *read, *write);
    break;
  case PV_FLOAT32:
    return mInterface.update_samples<float,1>(param, *read, *write);
    break;
  case PV_FLOAT64:
    return mInterface.update_samples<double,1>(param, *read, *write);
    break;

  case PV_RGB:
    return mInterface.update_samples<char,3>(param, *read, *write);
    break;
  case  PV_RGBA:
    return mInterface.update_samples<char,4>(param, *read, *write);
    break;
    
  }    
    
  return false;
}

//! Access the sample nearest to the given position 
int  VisusFastLoopDataSource::accessSample(const std::vector<double>& position,
                                           const int field_index, 
                                           const VisusTime& current_time,
                                           std::vector<int>& index, 
                                           void* result)
{
  if (!isValid()) {
    vwarning("Trying to acess invalid data source");
    return 0;
  }

  std::vector<double> start(3),stop(3); // The requested bounding box in index space
  std::vector<double> dummy(3);

  mapDomainToIndexSpace(position,position,start,stop,dummy);

  data_params param;

  // Compute the indices. Warning: this might be a threading hazard
  index[0] = MAX(mInterface.hzfile().boundingbox[0],
                 MIN(mInterface.hzfile().boundingbox[1]-1,start[0]));
  index[1] = MAX(mInterface.hzfile().boundingbox[2],
                 MIN(mInterface.hzfile().boundingbox[3]-1,start[1]));
  index[2] = MAX(mInterface.hzfile().boundingbox[4],
                 MIN(mInterface.hzfile().boundingbox[5]-1,start[2]));
  
  param.flags |= BITMASK_FLAG;

  param.start = param.end = 0;
  
  param.range[0] = param.range[1] = index[0];
  param.range[2] = param.range[3] = index[1];
  param.range[4] = param.range[5] = index[2];

  if (!accessData(param, mWriteBuffer)) {
    index[0] = index[1] = index[2] = -1;
    return 0;
  }

  
  memcpy(result,mWriteBuffer->buffer,fieldSampleSize(field_index));

  return 1;
}


// Convert a value to a double.
double  VisusFastLoopDataSource::toDouble(const uint8_t* value, PvDataType dtype)
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
            v = (double) *(  (int32_t *)value);
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

void VisusFastLoopDataSource::computeMinMaxBounds(data_result* buffer, double &min_f, double& max_f)
{
  int sample_size = buffer->nbytes;
  int num_samples = buffer->size / sample_size;
  PvDataType field_type = fieldType(0);
  uint8_t* data = buffer->buffer;
  uint8_t* mask = buffer->bitmask;
  double value;

  min_f = sHugeValue;
  max_f = -sHugeValue;

  for (int i=0;i<num_samples;i++) {
    if ((*mask | (1 << (i%8))) != 0) {
      value = toDouble(data,field_type);
      min_f = MIN(min_f,value);
      max_f = MAX(max_f,value);
    }

    data += sample_size;
    if (i%8 == 7)
      mask += 1;
  }
   
}


#else  

////////////////////////////////////////////////////////////////
//////// Dummy implementation without idx interface ////////////
////////////////////////////////////////////////////////////////
 
VisusFastLoopDataSource::VisusFastLoopDataSource(const std::string& datainfo) : 
  VisusDataSource(), mValid(false)
{
}

VisusFastLoopDataSource::~VisusFastLoopDataSource()
{
}

int VisusFastLoopDataSource::interrupt() 
{
  return 0;
}

std::vector<int> VisusFastLoopDataSource::samples() const
{
  return std::vector<int>(3,0);
}

int VisusFastLoopDataSource::dimensions() const
{
  return 1;
}

void VisusFastLoopDataSource::boundingBox(std::vector<int>& left_lower, 
                                          std::vector<int>& right_upper) const
{
  left_lower = std::vector<int>(3,0);
  right_upper = std::vector<int>(3,0);
}

int VisusFastLoopDataSource::numberOfFields() const 
{
  return 0;
}

PvDataType VisusFastLoopDataSource::fieldType(int i) const
{
  return PV_RAW;
}

int VisusFastLoopDataSource::fieldSampleSize(int i) const
{
  return 1;
}

void VisusFastLoopDataSource::fieldRange(int i, double& min_f, double& max_f) const
{
  min_f = 0;
  max_f = 0;
}

std::string VisusFastLoopDataSource::fieldName(int i) const
{
  return std::string("");
}


int VisusFastLoopDataSource::accessAlignedData(const std::vector<double>& left_lower,
                                               const std::vector<double>& right_upper,
                                               const std::vector<int>& start_strides,
                                               const std::vector<int>& end_strides,
                                               const std::vector<int>& strides,
                                               const int field_index,
                                               const VisusTime& current_time,
                                               VisusBlockData& data,
                                               VisusReadWriteLock& access_lock)
{
  return 0;
}

int VisusFastLoopDataSource::accessSample(const std::vector<double>& position,
                                          const int field_index, 
                                          const VisusTime& current_time,
                                          std::vector<int>& index, 
                                          void* result)
{
  return 0;
}

#endif
