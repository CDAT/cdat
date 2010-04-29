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


#include <math.h>

#include "VisusQuaternion.h"
#include "VisusTransformation3D.h"

const VisusQuaternion VisusQuaternion::identity=VisusQuaternion(1.0,0.0,0.0,0.0);


VisusQuaternion::VisusQuaternion()
{
  mW = 1;
  mX = mY = mZ = 0;
}

VisusQuaternion::VisusQuaternion(const VisusQuaternion& quat)
{
  *this = quat;
}

VisusQuaternion::VisusQuaternion(float w, float x, float y, float z)
{
  mW = w;
  mX = x;
  mY = y;
  mZ = z;
}

VisusQuaternion& VisusQuaternion::operator==(const VisusQuaternion& quat)
{
  mW = quat.mW;
  mX = quat.mX;
  mY = quat.mY;
  mZ = quat.mZ;

  return *this;
}

void VisusQuaternion::fromMatrix(const VisusTransformation3D& matrix)
{
  float trace;
  float s;
  int i,j,k;
  float q[4];

  trace = matrix[0] + matrix[5] + matrix[10];

  if (trace+1 >= 0) {
    s = sqrt(trace + 1);
    mW = 0.5*s;

    s = 0.5 / s;
    
    mX = (matrix[9] - matrix[6]) * s;
    mY = (matrix[2] - matrix[8]) * s;
    mZ = (matrix[4] - matrix[1]) * s;
  }
  else {
    
    // The three diagonal elements
    i = 0;
    j = 1;
    k = 2;

    if (matrix[4*j+j] > matrix[4*i+i]) {
      i = 1;
      j = 2;
      k = 0;
    }
    
    if (matrix[10] > matrix[4*i+i]) {
      i = 2;
      j = 0;
      k = 1;
    }

    s = sqrt(matrix[4*i+i] - matrix[4*j+j] - matrix[4*k+k] + 1);
    
    q[i] = s*0.5;
    
    if (s != 0)
      s = 0.5 / s;
      
    q[3] = (matrix[4*j + k] - matrix[4*k + j]) * s;
    q[j] = (matrix[4*i + j] - matrix[4*j + i]) * s;
    q[k] = (matrix[4*i + k] - matrix[4*k + i]) * s;

    mX = q[0];
    mY = q[1];
    mZ = q[2];

    mW = q[3];
  }

  /*
  s = sqrt(mW*mW + mX*mX + mY*mY + mZ*mZ);
  mW /= s;
  mX /= s;
  mY /= s;
  mZ /= s;
  */
}


void VisusQuaternion::toMatrix(VisusTransformation3D& matrix) const
{
  
  float x2 = mX + mX;
  float y2 = mY + mY;
  float z2 = mZ + mZ;

  float xx = mX * x2;
  float xy = mX * y2;
  float xz = mX * z2;

  float yy = mY * y2;
  float yz = mY * z2;

  float zz = mZ * z2;
  
  float wx = mW * x2;
  float wy = mW * y2;
  float wz = mW * z2;

  matrix[0] = 1 - (yy + zz);
  matrix[1] = xy - wz;
  matrix[2] = xz + wy;
  matrix[3] = 0;

  matrix[4] = xy + wz;
  matrix[5] = 1 - (xx + zz);
  matrix[6] = yz - wx;
  matrix[7] = 0;

  matrix[8] = xz - wy;
  matrix[9] = yz + wx;
  matrix[10]= 1 - (xx + yy);
  matrix[11]= 0;

  matrix[12] = 0;
  matrix[13] = 0;
  matrix[14] = 0;
  matrix[15] = 1;
  

  /*
  matrix[0] = 1 - 2*mY*mY - 2*mZ*mZ;
  matrix[1] = 2*mX*mY + 2*mZ*mW;
  matrix[2] = 2*mX*mZ - 2*mY*mW;
  matrix[3] = 0;

  matrix[4] = 2*mX*mY - 2*mZ*mW;
  matrix[5] = 1 - 2*mX*mX - 2*mZ*mZ;
  matrix[6] = 2*mY*mZ + 2*mX*mW;
  matrix[7] = 0;

  matrix[8] = 2*mX*mZ + 2*mY*mW;
  matrix[9] = 2*mY*mZ - 2*mX*mW;
  matrix[10] = 1 - 2*mX*mX - 2*mY*mY;
  matrix[11] = 0;
  
  matrix[12] = 0;
  matrix[13] = 0;
  matrix[14] = 0;
  matrix[15] = 1;
  */
}



VisusQuaternion slerp(const VisusQuaternion& q1, VisusQuaternion q2, float t)
{
  VisusQuaternion result;
  float cos_theta;
  float theta;
  float sin_theta;

  cos_theta = q1.mW*q2.mW + q1.mX*q2.mX + q1.mY*q2.mY +q1.mZ*q2.mZ;
  
  if (cos_theta < 0) {
    cos_theta *= -1;
    q2.mW *= -1;
    q2.mX *= -1;
    q2.mY *= -1;
    q2.mZ *= -1;
  }

  if (fabs(cos_theta) >= 1) {
    result = q1;
    return result;
  }

  theta = acos(cos_theta);
  sin_theta = sqrt(1 - cos_theta*cos_theta);

  if (fabs(sin_theta) < 0.00001) {
    result.mW = 0.5*(q1.mW + q2.mW); 
    result.mX = 0.5*(q1.mX + q2.mX); 
    result.mY = 0.5*(q1.mY + q2.mY); 
    result.mZ = 0.5*(q1.mZ + q2.mZ); 
    
    return result;
  }

  float r1,r2;

  r1 = sin((1-t)*theta) / sin_theta;
  r2 = sin(t*theta) / sin_theta;

  result.mW = (r1*q1.mW + r2*q2.mW);
  result.mX = (r1*q1.mX + r2*q2.mX);
  result.mY = (r1*q1.mY + r2*q2.mY);
  result.mZ = (r1*q1.mZ + r2*q2.mZ);

  return result;
}
