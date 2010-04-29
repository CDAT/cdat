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


#ifndef VISUSNULLDATASOURCE_H
#define VISUSNULLDATASOURCE_H

#include <string>

#include "VisusDataSource.h"

class VisusNullDataSource : public VisusDataSource
{
public:

  VisusNullDataSource() {}

  VisusNullDataSource(const std::string& description) {}

  virtual ~VisusNullDataSource() {}

  virtual bool isValid() const {return false;}
  
  virtual int interrupt() const {return 0;}

  virtual std::vector<int> samples() const {return std::vector<int>(3,0);}

  virtual int dimensions() const {return 3;}

  virtual void boundingBox(std::vector<int>& left_lower, std::vector<int>& right_upper) const {}

  virtual int numberOfFields() const {return 0;}
  
  virtual PvDataType fieldType(int i) const {return PV_RAW;}

  virtual int fieldSampleSize(int i) const {return 0;}

  virtual void fieldRange(int i, double& min_f, double& max_f) const {}

  virtual std::string fieldName(int i) const {return "";}
  
  //! Return the physical unit of the domain
  virtual VisusUnit unit() const {return VISUS_UNDEFINED_UNIT;}

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
  virtual int accessAlignedData(const std::vector<double>& left_lower,
                                const std::vector<double>& right_upper,
                                const std::vector<int>& start_strides,
                                const std::vector<int>& end_strides,
                                const std::vector<int>& strides,
                                const int field_index,
                                const VisusTime& current_time,
                                VisusBlockData& data,
                                VisusReadWriteLock& access_lock) {return 0;}

  //! Access the sample nearest to the given position 
  virtual int accessSample(const std::vector<double>& position,
                           const int field_index, 
                           const VisusTime& current_time,
                           std::vector<int>& index, 
                           void* result) {return 0;}
};




#endif
