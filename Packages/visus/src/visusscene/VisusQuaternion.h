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


#ifndef VISUSQUATERNION_H
#define VISUSQUATERNION_H

class VisusTransformation3D;

class VisusQuaternion
{
public:
  
  friend VisusQuaternion slerp(const VisusQuaternion& q1, VisusQuaternion q2,float t);

  static const VisusQuaternion identity;
  static VisusQuaternion rotateX(float rad);
  static VisusQuaternion rotateY(float rad);
  static VisusQuaternion rotateZ(float rad);
  static VisusQuaternion rotateAxis(const float axis[3],float rad);

  VisusQuaternion();
  VisusQuaternion(const VisusQuaternion& quat);
  VisusQuaternion(float w, float x, float y, float z);
  
  ~VisusQuaternion() {}

  VisusQuaternion& operator==(const VisusQuaternion& quat);

  void fromMatrix(const VisusTransformation3D& matrix);
  void toMatrix(VisusTransformation3D& matrix) const;
 
private:

  float mW;
  float mX;
  float mY;
  float mZ;
};

VisusQuaternion slerp(const VisusQuaternion& q1, VisusQuaternion q2,float t); 


#endif
