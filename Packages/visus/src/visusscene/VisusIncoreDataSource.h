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


#ifndef INCOREDATASOURCE_H
#define INCOREDATASOURCE_H

#include <vector>
#include "VisusDataSource.h"
#include "VisusIntTime.h"

/*! An incore data source implements the data source interface for
 *  array(s) in main memory. A data set in memory is described as
 *  follows:
 *
 *  <int>  <int>  <int>  <int>  <int> [property0] .. [propertyn] { <field0> } ... { <fieldn> }
 *  x-dim  y-dim  z-dim  t-dim  unit
 *  and properties can be
 *  domain <double> <double> <double> <double> <double> <double> 
 *  time <int> <start> <stop>  // TimeType time-begin time-end 
 * 
 *  and fields are described as 
 *  <fieldx>: <string>     <int>      <int>       <int>     [atribute0] ... [attributen]
 *              name    pointer-to-  datatype  samplesize
 *                        uchar
 *  and attibutes can be 
 *  range <double> <double>
 *  mask <int>
 *  
 */
class VisusIncoreDataSource : public VisusDataSource 
{

public:
  
  VisusIncoreDataSource(const std::string& args);

  virtual ~VisusIncoreDataSource();

  bool isValid() const;
  
  int interrupt() {return 1;}

  std::vector<int> samples() const {return mSamples;}

  int dimensions() const {return mSamples.size()-1;}

  void boundingBox(std::vector<int>& left_lower, std::vector<int>& right_upper) const;

  int numberOfFields() const {return mData.size();}
  
  PvDataType fieldType(int i) const;

  int fieldSampleSize(int i) const;

  void fieldRange(int i, double& min_f, double& max_f) const;

  std::string fieldName(int i) const;
  
  //! Return the physical unit of the domain
  virtual VisusUnit unit() const {return mUnit;}

  //! Return the physical bounding box of the domain in unit's
  virtual void unitBoundingBox(std::vector<double>& left_lower, std::vector<double>& right_upper) const {}

  //! Access axis aligned data
  /*! Access axis aligned data based on a desired bounding box and
   *  resolution. The bounding box is given in the "native" coordinate
   *  system of the data.
   * 
   *  @param[in] left_lower: left lower corner of the bounding box 
   *  @param[in] right_upper: right upper corner of the bounding box
   *  @param[in] start_strides: lowest resolution of data valid for 
   *                            this query
   *  @param[in] end_strides: highest resolution necessary for this 
   *                          query
   *  @param[in] strides: current resolution we want
   *  @param[in] field_index: index of the scalar field we want
   *  @param[in] current_time: current time of the scalar field we want
   *  @param[out] data: brick of data to contain the results
   *  @param[in] access_lock: read/write lock that protects access to 
   *                          data
   *  
   */
  int accessAlignedData(const std::vector<double>& left_lower,
                        const std::vector<double>& right_upper,
                        const std::vector<int>& start_strides,
                        const std::vector<int>& end_strides,
                        const std::vector<int>& strides,
                        const int field_index,
                        const VisusTime& current_time,
                        VisusBlockData& data,
                        VisusReadWriteLock& access_lock);
 
  //! Access the sample nearest to the given position 
  int accessSample(const std::vector<double>& position,
                   const int field_index, 
                   const VisusTime& current_time,
                   std::vector<int>& index, 
                   void* result);
protected:

  //! Internal struct to store the field ranges
  struct FieldRange {
    double r[2];
  };

  //! Pointer to the data arrays
  std::vector<unsigned char*> mData;

  //! Pointer to the mask arrays
  std::vector<unsigned char*> mMask;

  //! The names of the fields
  std::vector<std::string> mFieldName;

  //! The field ranges
  std::vector<FieldRange> mFieldRange;

  //! Data types
  std::vector<PvDataType> mDataType;

  //! Data size of raw data
  std::vector<int> mDataSize;

  //! Number of samples in each dimension
  std::vector<int> mSamples;

  //! Protected constructore to avoid initalization
  VisusIncoreDataSource() : VisusDataSource() {}
  
  //! Compute the start of the data of the current time step
  virtual int timeOffset(const VisusTime& current_time);

  //! Split up the given string into substrings using " " as delimiter
  /*! This function splits up a string into multiple sub-strings using
   *  " " as delimiter. Furthermore, it will also strip all leading
   *  and trailing copies of the delimiter and return an array of
   *  resulting tokens
   *  @param str: the input string
   *  @return vector of tokens
   */
  std::vector<std::string> tokenize(const std::string& str) const;

  //! Initialize an incore data source using the standard string
  int initializeFromString(std::vector<std::string>& tokens);

  //! Compute the range of the given array
  void computeMinMaxBound(unsigned char* data, PvDataType data_type,
                          int data_size,double& min_val, double& max_val);

  //! Parse and store the field information of a single field
  /*! Parse the field information between a single pair of {} and if
   *  the field is read in constistently store the information.
   *  @param it: The token iterator pointing to the next token to be
   *             processed
   *  @param tokens: The vector of all tokens
   *  @return: 1 if a field was successfullly read and stored; and 0 
   *           otherwise
   */
  int parseFieldData(std::vector<std::string>::iterator& it,
                     const std::vector<std::string>& tokens);

  template <typename DataType>
  int copySamples(const std::vector<int>& left,const std::vector<int>& right,
                  const std::vector<int>& strides,const std::vector<int>& samples,
                  int time_offset, int field_index, VisusBlockData& data);
};


template <typename DataType>
int VisusIncoreDataSource::copySamples(const std::vector<int>& left,const std::vector<int>& right,
                                       const std::vector<int>& strides,const std::vector<int>& samples,
                                       int time_offset, int field_index, VisusBlockData& data)
{
  DataType* buffer = (DataType*)mData[field_index];
  DataType* target = (DataType*)data.data();
    
  for (int i=left[2];i<=right[2];i+=strides[2]) {
    for (int j=left[1];j<=right[1];j+=strides[1]) {
      for (int k=left[0];k<=right[0];k+=strides[0]) {
        *target = *(buffer + k +j*mSamples[0] + i*mSamples[1]*mSamples[0] + time_offset);
        target++;
      }
    }
  }
 
  return 1;
}

#endif
