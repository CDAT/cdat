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

#ifndef VISUS_INCORE_ENCODER
#define VISUS_INCORE_ENCODER

#include <string>
#include <vector>

#include "PvDataType.h"
#include "VisusUnits.h"

class VisusIncoreEncoder
{
public:
  static const double UNDEFINED;

  //! Initialize the incore data
  VisusIncoreEncoder(const int xdim, const int ydim, const int zdim, int tdim, const VisusUnit unit=VISUS_METERS);

  VisusIncoreEncoder(const std::string dataDescription);

  //! Set the domain of the incore data
  bool domain(const std::vector<float>& left, const std::vector<float>& right);

  //! Set the domain of the incore data
  bool domain(const float leftlowerx, const float leftlowery, const float leftlowerz,
              const float rightupperx, const float rightuppery, const float rightupperz);

  //! Set the unit of the source
  void unit(VisusUnit unit) {mUnit = unit;}

  //! Add generic field to incore data
  void field (void* data, PvDataType type, const int size, 
              const std::string name="Unknown", const double minValue=UNDEFINED, const double maxValue=UNDEFINED);

  //! Add field to incore data
  void field (float* data, const std::string name="Unknown", 
              const double minValue=UNDEFINED, const double maxValue=UNDEFINED);

  //! Add field to incore data
  void field (double* data, const std::string name="Unknown", 
              const double minValue=UNDEFINED, const double maxValue=UNDEFINED);

  //! Add field to incore data
  void field (int* data, const std::string name="Unknown", 
              const double minValue=UNDEFINED, const double maxValue=UNDEFINED);

  //! Change the address of specified field
  bool changeFieldAddress(int index, void* data);

  //! Return encoded string for incore data
  std::string toString(bool withAddress=true) const;

  //! Get num fields
  int numFields() { return mFieldAddress.size(); }

  //! Get field size
  int getFieldSize(int index);

  //! Get field address
  void* getFieldAddress(int index);

private:
  int mXdim, mYdim, mZdim, mTdim;
  std::vector<float> mLeft, mRight;
  std::vector<void*> mFieldAddress;
  std::vector<std::string> mFieldName;
  std::vector<PvDataType> mFieldType;
  std::vector<int> mFieldSize;
  std::vector<double> mFieldMin;
  std::vector<double> mFieldMax;
  VisusUnit mUnit;
};


#endif

