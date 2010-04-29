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


#ifndef VISUSDATASOURCE_H
#define VISUSDATASOURCE_H

#include <vector>
#include <string>

#include "VisusBlockData.h"
#include "VisusReadWriteLock.h"
#include "VisusUnits.h"
#include "VisusBoundingBox.h"
#include "VisusGlobalTime.h"


class VisusDataSource
{
public:

  VisusDataSource();

  virtual ~VisusDataSource() {}

  //! If the current data source valid and can it be used
  virtual bool isValid() const = 0;
  
  //! Interrupt the current extraction for a new data request
  virtual int interrupt() {return 1;}

  //! The number of samples in each dimension for this data source
  virtual std::vector<int> samples() const = 0;

  //! Return how many dimensions this data set provides
  virtual int dimensions() const = 0;

  //! The bounding box in index space 
  virtual void boundingBox(std::vector<int>& left_lower, std::vector<int>& right_upper) const = 0;

  //! Return the number of different fields this data source provides
  virtual int numberOfFields() const = 0;
  
  //! Return the field type of the i'th field
  virtual PvDataType fieldType(int i) const = 0;

  //! Return the samples size of the i'th field
  virtual int fieldSampleSize(int i) const = 0;

  //! Return the range of the i'th field
  virtual void fieldRange(int i, double& min_f, double& max_f) const = 0;

  //! Return the name o fthe i'th field
  virtual std::string fieldName(int i) const = 0;

  //! Return the physical unit of the domain
  virtual VisusUnit unit() const {return mUnit;}

  //! Return the physical bounding box of the domain in unit coordinates
  virtual void domainBoundingBox(std::vector<double>& left_lower, std::vector<double>& right_upper) const;

  //! Return the physical bounding box of the domain in unit coordinates
  virtual void domainBoundingBox(VisusBoundingBox& bbox) const;

  //! Return the time information (range, resolution, last used time step)
  VisusGlobalTime timeInfo() const {return mTimeInfo;}

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
  virtual int accessAlignedData(const std::vector<double>& left_lower,
                                const std::vector<double>& right_upper,
                                const std::vector<int>& start_strides,
                                const std::vector<int>& end_strides,
                                const std::vector<int>& strides,
                                const int field_index,
                                const VisusTime& current_time,
                                VisusBlockData& data,
                                VisusReadWriteLock& access_lock) = 0;
  
  //! Convinience wrapper around the accessAlignedData function
  /*! This function provides a simpler wrapper around the
   *  accessAlignedData function. It is designed to allow easy access
   *  to a certain piece of data at a single resolution without
   *  protection against threading issues. The caller must guarantee
   *  that data is not modified during the call.
   *  @param[in] left_lower: left lower corner of the bounding box 
   *  @param[in] right_upper: right upper corner of the bounding box
   *  @param[in] strides: resolution of data we want
   *  @param[in] field_index: index of the scalar field we want
   *  @param[in] current_time: current time of the scalar field we want
   *  @param[out] data: brick of data to contain the results
   *  
   */
  virtual int accessAlignedData(const std::vector<double>& left_lower,
                                const std::vector<double>& right_upper,
                                const std::vector<int>& strides,
                                const int field_index,
                                const VisusTime& current_time,
                                VisusBlockData& data);
                                


  //! Access the sample nearest to the given position 
  /*! Return the sample closest to the given position as well as its
   *  location in index space. Note that unline accessAlignedData this
   *  function will clamp the request to the bounding box of the data
   *  and always return a valid sample. Another significant difference
   *  is that this function is NOT threadsafe. It will write the
   *  result whenever it is finished extracting it without any
   *  locking.
   *  @param position: the requested position in physical space
   *  @param field_index: index o fthe field to use
   *  @param current_time: current time of the scalar field we want
   *  @param index: coordinates of the returned sample in index space
   *  @param result: pointer to where the resulting data should be 
   *  @param         written
   */
  virtual int accessSample(const std::vector<double>& position,
                           const int field_index,   
                           const VisusTime& current_time,
                           std::vector<int>& index, 
                           void* result) = 0;
protected:

  //! What units are used to describe the physical space covered by the
  //! data, see VisusUnits.h
  VisusUnit mUnit;

  //! Left lower corner of the bounding box in physical units
  std::vector<double> mLeftLower;
  
  //! Right upper corner of the bounding box in physical units
  std::vector<double> mRightUpper;

  //! Width a grid cell in physical units for cell centered data
  /*! This array stores the width of a cell when treating the
   *  data as cell centered. We assume the *center* of the first cell
   *  lies at mLeftLower and the *center* of the last cell at
   *  mRightUpper. The width of these cells will be often used to
   *  correct the differences between vertex centered displays
   *  (e.g. iso-surfaces) and cell centered displays (e.g. slices)
   */ 
  std::vector<double> mCellSize;
  
  //! Information about the time interval, resolution, and current time
  VisusGlobalTime mTimeInfo;

  //! Map a bounding box from domain space into index space
  /*! Given a bounding box from domain space map it into index space
   *  using the global physical bounding box. Note, that this function
   *  will *not* clamp the index space coordinates to the samples-size
   *  box but will round the indices appropriately
   *  @param left_lower: left lower corner of the bbox in physical space
   *  @param right_upper: right upper corner of the bbox in physical space
   *  @param min_index: left lower corner of the bbox in index space
   *  @param max_index: right upper corner of the bbox in index space
   *  @param zoom: the length ratios between the input bounding box 
   *               in index space and the domain bounding box in physical 
   *               space
   *  @return 1 if successful | 0 otherwise
   */
  int mapDomainToIndexSpace(const std::vector<double>& left_lower, 
                            const std::vector<double>& right_upper,
                            std::vector<double>& min_index, 
                            std::vector<double>& max_index,
                            std::vector<double>& zoom);
};


#endif
