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

#include "xmlParser.h"

#include "VisusGraph.h"
#include "VisusBlockData.h"


VisusGraph::VisusGraph(const VisusNodeType nodeType):VisusBorderBox2D(nodeType)
{  
  mUnitLength = 0.5;
  mXUnits = 1.0;
  mYUnits = 1.0;

  mXAxis.enable();
  mXAxis.majorTickLength(0.10);
  mXAxis.minorTickLength(0.50);
  
  mYAxis.enable();
  mYAxis.majorTickLength(0.10);
  mYAxis.minorTickLength(0.50);

  computeSize();
}  

VisusGraph::~VisusGraph()
{}

void VisusGraph::computeSize()
{
  mWidth = mUnitLength * mYUnits;
  mLength= mUnitLength * mXUnits;
}

bool VisusGraph::unitLength(const float length)
{
  mUnitLength = length;
  computeSize();
  return true;
}

bool VisusGraph::xUnits(const float x)
{
  mXUnits = x;
  computeSize();
  return true;
}

bool VisusGraph::yUnits(const float y)
{
  mYUnits = y;
  computeSize();
  return true;
}


//==============================================================
// DataFunctor
//==============================================================
bool GraphHelper::DataFunctor::call(const VisusBlockData& data)
{
  // Validate Data Type
  switch (data.dataType())
  {
  case PV_FLOAT:
  case PV_FLOAT32:
  case PV_FLOAT64:
    break;
  default:
    vwarning("unsupported data type given as data. currently support various PV_FLOAT types",NULL);
    return false;
  }

  // Validate Dimensions
  std::vector<int> dims = data.samples();
  verror(dims[1]!=2,"invalid VisusBlockData given. Y-Dimension should be 2 to contain X,Y value of point in line", false);
  verror(dims[2]!=1,"invalid VisusBlockData given. Z-Dimension should be 1",false);

  // Get size of data
  numPoints = dims[0];

  switch (data.dataType())
  {
    case PV_FLOAT:
    case PV_FLOAT32:
    {
      const float* fdata = reinterpret_cast<const float*>(data.data());
      for (int i=0,k=0; i<numPoints; ++i) {
        float x = fdata[k++];
        float y = fdata[k++];
        process(x,y);
      }
    }  
    break;

    case PV_FLOAT64:
    {
      const double* ddata = reinterpret_cast<const double*>(data.data());
      for (int i=0,k=0; i<numPoints; ++i) {
        double x = ddata[k++];
        double y = ddata[k++];
        process(x,y);
      }
    }
    break;
  }
  return true;
}

//==============================================================
// DistanceFunctor
//==============================================================
GraphHelper::DistanceFunctor::DistanceFunctor(const float minXLabel, const float maxXLabel)
{
  mSmallDistance = maxXLabel - minXLabel;
  mSumDistance = 0.0;
  mLastX = minXLabel;
}

float GraphHelper::DistanceFunctor::avgDist() {
  return mSumDistance / (numPoints-1);
}

float GraphHelper::DistanceFunctor::smallDist() {
  return mSmallDistance;
}

void GraphHelper::DistanceFunctor::process(float x, float y)
{
  float dist = x - mLastX;
  if (dist < mSmallDistance && dist > 0) mSmallDistance = dist;
  mLastX = x;
  mSumDistance += dist;
}

//==============================================================
// MinMaxFunctor
//==============================================================
GraphHelper::MinMaxFunctor::MinMaxFunctor(const bool computeX, VisusBorderAxis& x, 
                                          const bool computeY, VisusBorderAxis& y)
  : mComputeX(computeX),mX(x),mComputeY(computeY),mY(y),mSet(false)
{}

void GraphHelper::MinMaxFunctor::process(float x, float y)
{
  if (! mSet) {
    if (mComputeX) {
      mX.minValue(x);
      mX.maxValue(x);
    }
    if (mComputeY) {
      mY.minValue(y); 
      mY.maxValue(y);
    }
    mSet = true;
  }
  else {
    if (mComputeX) {
      if (x > mX.maxValue()) mX.maxValue(x);
      else if (x < mX.minValue()) mX.minValue(x);
    }
    if (mComputeY) {
      if (y > mY.maxValue()) mY.maxValue(y);
      else if (y < mY.minValue()) mY.minValue(y);
    }
  }
}

void VisusGraph::toXMLLocalVariables(XMLNode& node)
{
  VisusBorderBox2D::toXMLLocalVariables(node);

  node.addAttribute("unitLength", mUnitLength);
  node.addAttribute("xUnits", mXUnits);
  node.addAttribute("yUnits", mYUnits);
}

bool VisusGraph::fromXMLLocalVariables(XMLNode& node)
{
  if (! VisusBorderBox2D::fromXMLLocalVariables(node)) {
    vwarning("failed to properly initialize parent class while loading xml");
    return false;
  }

  mUnitLength = xmltof(node.getAttribute("unitLength"), mUnitLength);
  mXUnits = xmltof(node.getAttribute("xUnits"), mXUnits);
  mYUnits = xmltof(node.getAttribute("yUnits"), mYUnits);

  return true;
}
