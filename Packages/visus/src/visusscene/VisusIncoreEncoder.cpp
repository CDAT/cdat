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

#include <sstream>
#include <cstdlib>
#include <cstring>

#include "VisusAssert.h"
#include "VisusIncoreEncoder.h"

const double VisusIncoreEncoder::UNDEFINED = 9999999.99;


VisusIncoreEncoder::VisusIncoreEncoder(const int xdim, const int ydim, const int zdim, int tdim, const VisusUnit unit)
{
  mXdim = xdim;
  mYdim = ydim;
  mZdim = zdim;
  mTdim = tdim; 
  mUnit = unit;
}

VisusIncoreEncoder::VisusIncoreEncoder(const std::string desc)
{
  std::string name;
  std::stringstream ss;
  ss << desc;

  // Read Header
  ss >> name;
  if (name.find("Incore") == std::string::npos) {
    vwarning("failed to properly initialize VisusIncoreEncoder with description string");
    return;
  }

  // Read Dimensions
  int u;
  ss >> mXdim >> mYdim >> mZdim >> mTdim >> u;

  mUnit = (VisusUnit)u;
  vverbose("VisusIncoreEncoder read dims %d %d %d %d unit %d\n", VISUS_XML_VERBOSE,
    mXdim, mYdim, mZdim, mTdim, mUnit);

  // Read Possible Domain
  ss >> name;
  if (name.find("domain") != std::string::npos)
  {
    mLeft.resize(3);
    mRight.resize(3);
    ss >> mLeft[0] >> mLeft[1] >> mLeft[2] 
       >> mRight[0] >> mRight[1] >> mRight[2];

    vverbose("VisusIncoreEncoder read domain %f %f %f %f %f %f\n", VISUS_XML_VERBOSE,
      mLeft[0], mLeft[1], mLeft[2], mRight[0], mRight[1], mRight[2]);

    ss >> name;
  }

  // Read Fields
  while (name.find("{") != std::string::npos)
  {
    ss >> name;
    mFieldName.push_back(name);

    ss >> name;
    mFieldAddress.push_back((void*)atoi(name.c_str()));

    ss >> name;
    mFieldType.push_back((PvDataType)atoi(name.c_str()));

    ss >> name;
    mFieldSize.push_back(atoi(name.c_str()));

    vverbose("VisusIncoreEncoder read field %s %d %d %d\n", VISUS_XML_VERBOSE,
      mFieldName[mFieldName.size()-1].c_str(), mFieldAddress[mFieldAddress.size()-1],
      mFieldType[mFieldType.size()-1], mFieldSize[mFieldSize.size()-1]);

    ss >> name;
    if (name.find("range") != std::string::npos)
    {
      ss >> name;
      mFieldMin.push_back(atof(name.c_str()));

      ss >> name;
      mFieldMax.push_back(atof(name.c_str()));

      vverbose("VisusIncoreEncoder read range %f %f\n", VISUS_XML_VERBOSE,
        mFieldMin[mFieldMin.size()-1], mFieldMax[mFieldMax.size()-1]);
      ss >> name;
    }
    else {
      mFieldMin.push_back(UNDEFINED);
      mFieldMax.push_back(UNDEFINED);
    }

    if (name.find("}") == std::string::npos)
    {
      vwarning("error processing data description. did not find expected end of field token");
      return;
    }
    ss >> name;
  }
  vverbose("Successfully loaded VisusIncoreEncoder from DataDescription\n", VISUS_XML_VERBOSE);
}

bool VisusIncoreEncoder::changeFieldAddress(int index, void* newAddress)
{
  if ((int)mFieldAddress.size() < index)
    return false;

  mFieldAddress[index] = newAddress;

  return true;
}

bool VisusIncoreEncoder::domain(const std::vector<float>& left, const std::vector<float>& right)
{
  if (left.size() != 3 || right.size() != 3)
    return false;

  mLeft.resize(3);
  mRight.resize(3);

  for (int i=0; i<3; ++i) {
    mLeft[i] = left[i];
    mRight[i]= right[i];
  }
  return true;
}

bool VisusIncoreEncoder::domain(const float leftlowerx, const float leftlowery, const float leftlowerz,
                                const float rightupperx, const float rightuppery, const float rightupperz)
{
  mLeft.resize(3);
  mRight.resize(3);

  mLeft[0] = leftlowerx;
  mLeft[1] = leftlowery;
  mLeft[2] = leftlowerz;

  mRight[0] = rightupperx;
  mRight[1] = rightuppery;
  mRight[2] = rightupperz;

  return true;
}

void VisusIncoreEncoder::field(void* data, const PvDataType type, const int size,
                               const std::string name, const double minValue, const double maxValue)
{
  mFieldAddress.push_back(data);
  mFieldName.push_back(name);
  mFieldType.push_back(type);
  mFieldSize.push_back(size);
  mFieldMin.push_back(minValue);
  mFieldMax.push_back(maxValue);
}

void VisusIncoreEncoder::field (float* data, const std::string name, const double minValue, const double maxValue)
{
  field(reinterpret_cast<void*>(data), PV_FLOAT, sizeof(float), name, minValue, maxValue);
}

void VisusIncoreEncoder::field (double* data, const std::string name, const double minValue, const double maxValue)
{
  field(reinterpret_cast<void*>(data), PV_FLOAT64, sizeof(double), name, minValue, maxValue);
}

void VisusIncoreEncoder::field (int* data, const std::string name, const double minValue, const double maxValue)
{
  field(reinterpret_cast<void*>(data), PV_INT, sizeof(int), name, minValue, maxValue);
}

std::string VisusIncoreEncoder::toString(bool withAddress) const
{
  std::stringstream ss;

  ss << "Incore: " << mXdim << " " << mYdim << " " << mZdim << " " << mTdim << " " << mUnit;
  
  if (mLeft.size() == 3) 
  {
    ss << " domain " 
       << mLeft[0] << " " << mLeft[1] << " " << mLeft[2] << " "
       << mRight[0]<< " " <<mRight[1] << " " <<mRight[2];
  }

  char addr[100];

  for (int i=0; i<(int)mFieldAddress.size(); ++i)
  {
    if (withAddress)
      sprintf(addr, "%d", mFieldAddress[i]);
    else
      strcpy(addr, "*");

    ss << " { " << mFieldName[i] << " " << addr << " " << mFieldType[i] << " " << mFieldSize[i];
    if (mFieldMin[i] != UNDEFINED && mFieldMax[i] != UNDEFINED) {
      ss << " range " << mFieldMin[i] << " " << mFieldMax[i];
    }
    ss << " }";
  }

  return ss.str();
}


int VisusIncoreEncoder::getFieldSize(int i)
{
  if (i >= (int)mFieldAddress.size()) {
    vwarning("attempt to get field size for field outside of bounds");
    return 0;
  }

  return mXdim * mYdim * mZdim * mTdim * mFieldSize[i];
}

void* VisusIncoreEncoder::getFieldAddress(int i)
{
  if (i >= (int)mFieldAddress.size()) {
    vwarning("attempt to get field size for field outside of bounds");
    return 0;
  }

  return mFieldAddress[i];
}
