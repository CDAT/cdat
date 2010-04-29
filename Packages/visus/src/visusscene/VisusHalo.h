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

#ifndef VISUSHALO_H
#define VISUSHALO_H

#include "VisusGroup.h"
#include "VisusSmartPointer.h"

class VisusHalo;
typedef VisusSmartPointer<VisusHalo> pVisusHalo;


enum VisusOpaqueness
{
  VISUS_OPAQUE_0=4,
  VISUS_OPAQUE_50=8,
  VISUS_OPAQUE_100=16
};

class VisusHalo : public VisusGroup
{
public:

  static pVisusHalo instantiate();

  VisusHalo();
  virtual ~VisusHalo();

  //! Set radius factor
  void radius(float factor) { mRadius = factor; }

  //! Set opaqueness 
  void opaqueness(VisusOpaqueness op) { mOpaqueness = op; }

  //! Set number of theta to draw (the more the more circular it appears)
  void numTheta(int n);

  //! Set the theta to begin drawing 
  void beginTheta(float theta);

  //! Set the theta to end drawing
  void endTheta  (float theta);

protected:
  void toXMLLocalVariables(XMLNode& node);
  bool fromXMLLocalVariables(XMLNode& node);

  //! Draw The Halo
  virtual void display3D(VisusTransformation3D model_view_3D);

private:
  VisusOpaqueness mOpaqueness;
  float mRadius;   // as magnifier of earth radius
  float* mMap;
  GLuint mTexID;
  int mLastType;
  int   mNumTheta;
  float mBeginTheta;
  float mEndTheta;
  float* mCosTheta;
  float* mSinTheta;

  float mZV[3];

  //! Render halo as screen door
  void renderScreenDoor(float cx, float cy, float z, float outerRadius, float innerRadius);

  //! Load rotation to make halo perpendicular to drawing direction
  void loadRotation(VisusTransformation3D model_view_3D);

  //! Load the shared color map as texture
  void loadColorMap();

  //! Load the cos and sin theta values for rendering
  void loadTheta();

  //! Cleanup theta arrays
  void deleteTheta();

  //! Draw a circle
  void drawCircle(float cx, float cy, float z, float outerRadius);
  void drawDonut(float cx, float cy, float z, float outerRadius, float innerRadius);
};



#endif
