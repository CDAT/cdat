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

#include "VisusTransformation2D.h"
#include "VisusAssert.h"
#include <math.h>

#ifndef M_PI  // This doesn't get defined in MSVC 9.0 via math.h
#define M_PI 3.14159265358979323846
#endif

VisusTransformation2D::VisusTransformation2D() : VisusTransformation<2>()
{
}

VisusTransformation2D::VisusTransformation2D(const VisusTransformation2D& t) : VisusTransformation<2>(t)
{
}

void VisusTransformation2D::rotate(float x, float y)
{
  float m[2][2];
  float theta;

  theta = mRotationSpeed*2*M_PI*y;

  m[0][0] = cos(theta)*mMatrix[0] + sin(theta)*mMatrix[3];
  m[0][1] = -sin(theta)*mMatrix[0] + cos(theta)*mMatrix[3];
  
  m[1][0] = cos(theta)*mMatrix[1] + sin(theta)*mMatrix[4];
  m[1][1] = -sin(theta)*mMatrix[1] + cos(theta)*mMatrix[4];
  
  mMatrix[0] = m[0][0];
  mMatrix[1] = m[1][0];
  mMatrix[3] = m[0][1];
  mMatrix[4] = m[1][1];
}

void VisusTransformation2D::translate(float x, float y)
{
  mMatrix[6] += mTranslationSpeed*x;
  mMatrix[7] += mTranslationSpeed*y;
}


VisusTransformation2D VisusTransformation2D::inverseMap() const
{
  float det;
  VisusTransformation2D inv;

  det = mMatrix[0]*mMatrix[4] - mMatrix[1]*mMatrix[3];
  
  if (fabs(det) < sFudgeFactor) {
    vwarning("2D Transformation matrix cannot be inverted ... returning ID");
    return inv;
  }

  inv[0] =  mMatrix[4] / det;
  inv[1] = -mMatrix[1] / det;

  inv[3] = -mMatrix[3] / det;  
  inv[4] =  mMatrix[0] / det;
  
  return inv;
}

VisusTransformation2D VisusTransformation2D::inverseTransform() const
{
  float det;
  VisusTransformation2D inv;

  det = mMatrix[0]*mMatrix[4] - mMatrix[1]*mMatrix[3];
  
  if (fabs(det) < sFudgeFactor) {
    vwarning("2D Transformation matrix cannot be inverted ... returning ID");
    return inv;
  }

  inv[0] =  mMatrix[4] / det;
  inv[1] = -mMatrix[1] / det;

  inv[3] = -mMatrix[3] / det;  
  inv[4] =  mMatrix[0] / det;

  inv[6] = -inv[0]*mMatrix[6] - inv[3]*mMatrix[7];
  inv[7] = -inv[1]*mMatrix[6] - inv[4]*mMatrix[7];
  
  
  return inv;
}
