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


#ifndef VISUSSPHERESLICE_H
#define VISUSSPHERESLICE_H

#include "VisusGroup.h"
#include "VisusConsumer.h"
#include "VisusSmartPointer.h"
#include "VisusTexture.h"
#include "VisusTransformation3D.h"

class VisusSphereSlice;
typedef VisusSmartPointer<VisusSphereSlice> pVisusSphereSlice;

enum SS_ORIENTATION 
{
  SS_LATITUDE,
  SS_LONGITUDE,

  SS_NON_SPHERE
};

enum SS_DIRECTION
{
  SS_NEGATIVE=-1,
  SS_POSITIVE=1
};

class VisusSphereSlice : public VisusGroup, public VisusConsumer
{
public:

  static pVisusSphereSlice instantiate();

  VisusSphereSlice();

  virtual ~VisusSphereSlice() {}

  //! Define number of stacks
  void stacks(int n) { mStacks = n; }

  //! Retrieve number of stacks
  int stacks() const { return mStacks; }

  //! Define number of slices
  void slices(int n) { mSlices = n; }

  //! Retrieve number of slices
  int slices() const { return mSlices; }

  //! Define orientation
  void orientation(const SS_ORIENTATION o) { mOrientation = o; }

  //! Retrieve orientation
  SS_ORIENTATION orientation() const { return mOrientation; }

  //! Define texture data X Mapping
  void dataXMapping(const SS_ORIENTATION xMapping) { mXMapping = xMapping; }

  //! Retrieve texture data X Mapping
  SS_ORIENTATION dataXMapping() const { return mXMapping; }

  //! Define texture data X Mapping
  void dataDirection(const SS_DIRECTION direction) { mDirection = direction; }

  //! Retrieve texture data direction
  SS_DIRECTION dataDirection() const { return mDirection; }

  //! Define offset from the sphere radius
  void radiusOffset(const float radiusOffset) { mRadiusOffset = radiusOffset; }

  //! Retrieve the offset from sphere radius
  float radiusOffset() const { return mRadiusOffset; }

  //! Define width of slice
  void width(const float w) { mWidth = w; }

  //! Retrieve the width of the slice
  float width() const { return mWidth; }
    
  //! Define start and stop coordinates
  bool span(const float startLatOrLong, const float degreeRotation);

  //! Retrieve start and stop coordinates
  std::vector<float> span() const;

  //! Define position of slice
  bool position(const float latOrLong);

  //! Retrieve the position of the slice
  float position() const { return mPosition; }

  //! Define the rotation function as rotating the request 
  virtual void rotate(float x, float y) {rotateRequest(x,y);}
  
  //! Define the translation function as translating the request
  virtual void translate(float x, float y) {translateRequest(x,y);}

  //! Rotate the data request which should be a no-op
  void rotateRequest(float x, float y) {return;}

  //! Translate the request normal to the current slice
  void translateRequest(float x, float y);

  //! Translate the request parallel to the current slice
  void shiftRequest(float x, float y);
  

  int connectInput(pVisusProducer producer);

protected:

  virtual void display3D(VisusTransformation3D model_view_3D = VisusTransformation3D());
private:

  void render();

  SS_ORIENTATION mOrientation;
  SS_ORIENTATION mXMapping;
  SS_DIRECTION mDirection;
  float mRadiusOffset;
  float mWidth;
  float mStart;
  float mRotation;
  float mPosition;
  int mSlices;
  int mStacks;

  //! The data stored as texture
  VisusTexture mData;
};



#endif
