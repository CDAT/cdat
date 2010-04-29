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


#ifndef VISUSGRAPH_H
#define VISUSGRAPH_H

#include "VisusBorderBox2D.h"

struct XMLNode;

class VisusGraph : public VisusBorderBox2D
{
 public:

  //! Destructor
  virtual ~VisusGraph();  

  //! Set unit length as percentage of window length (if vertical window height)
  bool unitLength(const float length);

  //! Set length given in units of x-direction
  bool xUnits(const float x);

  //! Set length given in units of y-direction
  bool yUnits(const float y);

  //! Get length of units in x-direction
  float unitLength() const { return mUnitLength; }

  //! Get length of units in x-direction
  float xUnits() const { return mXUnits; }

  //! Get length of units in x-direction
  float yUnits() const { return mYUnits; }

protected:
  VisusGraph(const VisusNodeType nodeType);

  void toXMLLocalVariables(XMLNode& node);
  bool fromXMLLocalVariables(XMLNode& node);

private:

  void computeSize();

  //! Length of a unit as defined as percentage of window length
  float mUnitLength;

  //! Length measured in units of x-direction
  float mXUnits;

  //! Length measured in units of y-direction
  float mYUnits;
};

#ifndef SWIG

class VisusBlockData;

namespace GraphHelper {

//! Base Functor which loops over and process block data in graph format
class DataFunctor
{
public:
  virtual ~DataFunctor() {}

  //! Process the block data, returns true on success
  bool call(const VisusBlockData& data);
protected:
  virtual void process(float x, float y)=0;
  int numPoints;
};

//! Functor which tracks various distances
class DistanceFunctor : public DataFunctor
{
public:
  DistanceFunctor(const float minXLabel, const float maxXLabel);
  float avgDist();
  float smallDist();
protected:
  void process(float x, float y);
private:
  float mLastX;
  float mSmallDistance;
  float mSumDistance;
};

//! Functor which tracks and resets min/max
class MinMaxFunctor : public DataFunctor
{
public:
  MinMaxFunctor(const bool computeX, VisusBorderAxis& x, const bool computeY, VisusBorderAxis& y);
protected:
  void process(float x, float y);
private:
  bool mComputeX;
  VisusBorderAxis& mX;
  bool mComputeY;
  VisusBorderAxis& mY;
  bool mSet;
};

};  // end of namespace

#endif // end of ndef SWIG

#endif
