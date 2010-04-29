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


#ifndef VISUSFASTLOOPDATASOURCE_H
#define VISUSFASTLOOPDATASOURCE_H

#include <string>
//#include "VisusStdInt.h"

#include "VisusDataSource.h"

#ifndef VISUS_DISABLE_IDX

#include "DataAccess.h"

#endif

class VisusFastLoopDataSource : public VisusDataSource
{
public:

  VisusFastLoopDataSource(const std::string& datainfo);
  
  virtual ~VisusFastLoopDataSource();
 
  
  /***************************************************************
   ******    Data Information                            *********
   **************************************************************/
  
  virtual bool isValid() const {return mValid;}
  
  virtual int interrupt();

  virtual std::vector<int> samples() const;

  virtual int dimensions() const;

  virtual void boundingBox(std::vector<int>& left_lower, std::vector<int>& right_upper) const;

  virtual int numberOfFields() const;
  
  virtual PvDataType fieldType(int i) const;

  virtual int fieldSampleSize(int i) const;

  virtual void fieldRange(int i, double& min_f, double& max_f) const;

  virtual std::string fieldName(int i) const;
  

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
                                VisusReadWriteLock& access_lock);

  //! Access the sample nearest to the given position 
  int accessSample(const std::vector<double>& position,
                   const int field_index, 
                   const VisusTime& current_time,
                   std::vector<int>& index, 
                   void* result);
  
private:
  

  static const double sHugeValue;

  bool mValid;

#ifndef VISUS_DISABLE_IDX
  DataAccess mInterface;
  data_result* mReadBuffer;
  data_result* mWriteBuffer;

  bool accessData(data_params& param, data_result* write);
  bool updateData(data_params& param,data_result* read, data_result* write);
  void computeMinMaxBounds(data_result* buffer, double &min_f, double& max_f);
  static double toDouble(const uint8_t* value, PvDataType dtype);

#endif  
};

#endif
