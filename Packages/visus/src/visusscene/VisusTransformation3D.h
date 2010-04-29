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


#ifndef VISUSTRANSFORM3D_H
#define VISUSTRANSFORM3D_H

#include <stdio.h>
#include <vector>
#include "VisusTransformation.h"
#include "VisusOpenGLState.h"

enum VisusTranslationStyle {
  FREE_TRANSLATION = 0,
  ALIGNED_TRANSLATION,
};

enum VisusSliceOrientation {
  VISUS_ORIENTATION_XY = 0,
  VISUS_ORIENTATION_XZ = 1,
  VISUS_ORIENTATION_YZ = 2,
};
  

class VisusTransformation3D : public VisusTransformation<3>
{
public:

  VisusTransformation3D();

  VisusTransformation3D(const VisusTransformation3D& t);

  virtual ~VisusTransformation3D() {}

  //! Assignement from a 4x4 row-major matrix
  VisusTransformation3D& operator=(const float mat[4][4]);

	VisusTransformation3D& operator=(const VisusTransformation3D& t);

  friend VisusTransformation3D operator*(const VisusTransformation3D& t1,
                                         const VisusTransformation3D& t2);
  friend VisusTransformation3D translationMatrix(float x, float y, float z);
  friend VisusTransformation3D scaleMatrix(float sx, float sy, float sz);

  VisusTranslationStyle translationStyle() const {return mTranslationStyle;}
  void translationStyle(VisusTranslationStyle style) {mTranslationStyle = style;}

  void rotate(const VisusTransformation3D& accumulate, float* center, float x, float y);
  
  void translate(const VisusTransformation3D& accumulate, 
                 const VisusOpenGLState & state, float x, float y);
  void translateFreely(const VisusTransformation3D& accumulate,
                       const VisusOpenGLState & state, float x, float y);
  void translateAligned(const VisusTransformation3D& accumulate,
                        const VisusOpenGLState & state, float x, float y);
  
  std::vector<float> translation(const VisusTransformation3D& accumulate, 
                                 const VisusOpenGLState & state,float x, float y);
  std::vector<float> freeTranslation(const VisusTransformation3D& accumulate, 
                                     const VisusOpenGLState & state,float x, float y);
  std::vector<float> alignedTranslation(const VisusTransformation3D& accumulate, 
                                        const VisusOpenGLState & state,float x, float y);

  void scale(float* center, float x, float y);

  void print();

  void normalize();

  VisusTransformation3D inverseMap() const;
  VisusTransformation3D inverseTransform() const;
  
  //! Write the matrix to file
  int save(FILE* output);

  //! Load the matrix from file
  int load(FILE* input);

private:
  
  VisusTranslationStyle mTranslationStyle;

  
};

VisusTransformation3D operator*(const VisusTransformation3D& t1,
                                const VisusTransformation3D& t2);

VisusTransformation3D translationMatrix(float x, float y, float z);
VisusTransformation3D scaleMatrix(float sx, float sy, float sz);

VisusTransformation3D xyPlane();
VisusTransformation3D xzPlane();
VisusTransformation3D yzPlane();

VisusTransformation3D slerp(VisusTransformation3D t1,VisusTransformation3D t2,
                            float t);


#endif
