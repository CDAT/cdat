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


#ifndef VISUSTRANSFORMATION_H
#define VISUSTRANSFORMATION_H

#include <cstring>
#include <sstream>
#include "xmlParser.h"

#include "VisusAssert.h"

/*! VisusTransformation implements a generic coordinate transformation
 *  represented as a (N+1)x(N+1) matrix. Furthermore, this class
 *  encapsulates the "freeze" transformation ability of the visus
 *  scenegraph meaning if frozen the current state is stored and a new
 *  matrix is modified. Finally, the transformation implements the two
 *  different styles of translation 1) translating with the view plane
 *  2) translating within the "coordinate" closest to the viewplane.
 *
 *  To be compatible with an OpenGL matrix the matrix is stored in
 *  column-major format so for a 4x4 matrix we have
 *
 *                  | m0 m4 m8  m12 |
 *     float M[16] =| m1 m5 m9  m13 |
 *                  | m2 m6 m10 m14 |
 *                  | m3 m7 m11 m15 |
 */

template<int Dimension>
class VisusTransformation 
{
public:

  static const char* XML_TAG;

  static const float sRotationSpeedDefault;
  static const float sTranslationSpeedDefault;
  static const float sScalingSpeedDefault;
  static const int   sMatrixDimension = Dimension+1;
  static const int   sMatrixSize = (Dimension+1)*(Dimension+1);
  static const float sFudgeFactor;

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/

  //! Default constructor
  VisusTransformation();

  //! Copy constructor
  VisusTransformation(const VisusTransformation& t);

  //! Default destructor
  virtual ~VisusTransformation() {}


  /***************************************************************
   ******                Operators                       *********
   **************************************************************/
 
  float &operator[](int i) {return mMatrix[i];}
  VisusTransformation& operator=(const VisusTransformation& t);
  bool operator==(const VisusTransformation& t);
  bool operator!=(const VisusTransformation& t);
  operator float*() {return mMatrix;}
  operator const float*() const {return mMatrix;}
  VisusTransformation& operator*=(const VisusTransformation &t);
  VisusTransformation& leftMultiply(const VisusTransformation& t);
  VisusTransformation& leftMultiply(const VisusTransformation* t);


  /***************************************************************
   ******                Member Access                   *********
   **************************************************************/

  float rotationSpeed() const {return mRotationSpeed;}
  float translationSpeed() const {return mTranslationSpeed;}
  float scalingSpeed() const {return mScalingSpeed;}

  /***************************************************************
   ******      Special Assignments                       *********
   **************************************************************/

  void setRotation(const VisusTransformation& t);
  void setTranslation(const VisusTransformation& t);

  /***************************************************************
   ******       Transformations                          *********
   **************************************************************/

  void map(const float *in, float *out) const;
  void inverseMap(const float *in, float *out) const;
  void transform(const float in[Dimension], float out[Dimension]) const;
  void transform(const double in[Dimension], double out[Dimension]) const;
  void inverseTransform(const float* in, float* out) const;

  virtual void scale(float x, float y); 

  //! Build instance data from XML data
  bool fromXML(XMLNode& node);

  //! Build XML data from instance data
  void toXML(XMLNode& parent) const;

private:

#ifdef SWIG_MAKE
  // SWIG needs to wrap the base class VisusTransformation<D>. In order to do this
  // this class must have an inverseTransform function defined.
  VisusTransformation inverseTransform() const {
    printf("error: base class inverseTransform() is being called\n");
    exit(1);
    return *this;
  } 
#endif

protected:

  float mMatrix[(Dimension+1)*(Dimension+1)];
  float mRotationSpeed;
  float mTranslationSpeed;
  float mScalingSpeed;

  float mag(float* v);
  float magSqr(float* v);
}; 
  

template<int Dimension>
const char* VisusTransformation<Dimension>::XML_TAG = "VisusTransformation";

template<int Dimension>
const float VisusTransformation<Dimension>::sRotationSpeedDefault = 0.5;

template<int Dimension>
const float VisusTransformation<Dimension>::sTranslationSpeedDefault = 10;

template<int Dimension>
const float VisusTransformation<Dimension>::sScalingSpeedDefault = 1;

template<int Dimension>
const float VisusTransformation<Dimension>::sFudgeFactor = 0.00001;

template<int Dimension>
VisusTransformation<Dimension>::VisusTransformation()
{
  memset(mMatrix,0,sizeof(float)*sMatrixSize);
  
  for (int i=0;i<sMatrixDimension;i++)
    mMatrix[i*sMatrixDimension + i] = 1;

  mRotationSpeed = sRotationSpeedDefault;
  mTranslationSpeed = sTranslationSpeedDefault;
  mScalingSpeed = sScalingSpeedDefault;
}



template<int Dimension>
VisusTransformation<Dimension>::VisusTransformation(const VisusTransformation& t)
{
  *this = t;
}


template<int Dimension>
void VisusTransformation<Dimension>::map(const float *in, float *out) const
{
  float tmp[3];
  for (int row=0;row<Dimension;row++) {
    tmp[row] = 0;
    for (int i=0;i<Dimension;i++) {
      tmp[row] += mMatrix[i*(Dimension+1) + row]*in[i];
    }
    tmp[row] += mMatrix[Dimension*(Dimension+1) + row];
  }
  out[0] = tmp[0];
  out[1] = tmp[1];
  out[2] = tmp[2];  
}

template<int Dimension>
void VisusTransformation<Dimension>::inverseMap(const float *in, float *out) const
{
  VisusTransformation<Dimension> inv;

  inv = inverseTransform();
  
  inv.map(in,out);
}

template<int Dimension>
void VisusTransformation<Dimension>::transform(const float in[Dimension], float out[Dimension]) const
{
  float tmp[Dimension]; // The temporaray value is necessary to all in == out
  for (int row=0;row<Dimension;row++) {
    tmp[row] = 0;
    for (int i=0;i<Dimension;i++) {
      tmp[row] += mMatrix[i*(Dimension+1) + row]*in[i];
    }
    tmp[row] += mMatrix[Dimension*(Dimension+1)+row];
  }

  out[0] = tmp[0];
  out[1] = tmp[1];
  out[2] = tmp[2];
}

template<int Dimension>
void VisusTransformation<Dimension>::transform(const double in[Dimension], double out[Dimension]) const
{
  double tmp[Dimension]; // The temporaray value is necessary to all in == out
  for (int row=0;row<Dimension;row++) {
    tmp[row] = 0;
    for (int i=0;i<Dimension;i++) {
      tmp[row] += mMatrix[i*(Dimension+1) + row]*in[i];
    }
    tmp[row] += mMatrix[Dimension*(Dimension+1)+row];
  }

  out[0] = tmp[0];
  out[1] = tmp[1];
  out[2] = tmp[2];
}


template<int Dimension>
void VisusTransformation<Dimension>::inverseTransform(const float *in, float *out) const
{
  VisusTransformation<Dimension> inv; 

  inv = inverseTransform();

  inv.transform(in,out);
}


template<int Dimension>
VisusTransformation<Dimension>& VisusTransformation<Dimension>::operator=(const VisusTransformation<Dimension>& t)
{
  memcpy(mMatrix,t.mMatrix,sizeof(float)*sMatrixSize);
  return *this;
}

template<int Dimension>
bool VisusTransformation<Dimension>::operator==(const VisusTransformation<Dimension>& t)
{
  return (memcmp(mMatrix,t.mMatrix,sizeof(float)*sMatrixSize) == 0);
}

template<int Dimension>
bool VisusTransformation<Dimension>::operator!=(const VisusTransformation<Dimension>& t)
{
  return (memcmp(mMatrix,t.mMatrix,sizeof(float)*sMatrixSize) != 0);
}

template<int Dimension>
VisusTransformation<Dimension>& VisusTransformation<Dimension>::operator*=(const VisusTransformation<Dimension> &t)
{
  float  m[(Dimension+1)*(Dimension+1)];

  for (int row=0;row<=Dimension;row++) {
    for (int column=0;column<=Dimension;column++) {
      m[column*(Dimension+1) + row] = 0;
      for (int i=0;i<=Dimension;i++) {
        m[column*(Dimension+1) + row] += mMatrix[i*(Dimension+1) + row]*t.mMatrix[column*(Dimension+1) + i];
      }
    }
  }
  memcpy(mMatrix,m,sizeof(float)*sMatrixSize);

  return *this;
}

template<int Dimension>
void VisusTransformation<Dimension>::scale(float x, float y)
{
  for (int row=0;row<Dimension;row++) {
    for (int column=0;column<Dimension;column++) {
      mMatrix[column*(Dimension+1) + row] *= mScalingSpeed*x;
    }
  }
}


template<int Dimension>
VisusTransformation<Dimension>& VisusTransformation<Dimension>::leftMultiply(const VisusTransformation &t)
{
  float  m[sMatrixSize];

  for (int row=0;row<=Dimension;row++) {
    for (int column=0;column<=Dimension;column++) {
      m[column*sMatrixDimension + row] = 0;
      for (int i=0;i<=Dimension;i++) {
        m[column*sMatrixDimension + row] += (mMatrix[column*sMatrixDimension + i]
                                             *t.mMatrix[i*sMatrixDimension+ row]);
      }
    }
  }
  memcpy(mMatrix,m,sizeof(float)*sMatrixSize);

  return *this;  
}

template<int Dimension>
VisusTransformation<Dimension>& VisusTransformation<Dimension>::leftMultiply(const VisusTransformation* t)
{
  float  m[sMatrixSize];

  for (int row=0;row<=Dimension;row++) {
    for (int column=0;column<=Dimension;column++) {
      m[column*sMatrixDimension + row] = 0;
      for (int i=0;i<=Dimension;i++) {
        m[column*sMatrixDimension + row] += (mMatrix[column*sMatrixDimension + i]
                                             *t->mMatrix[i*sMatrixDimension+ row]);
      }
    }
  }
  memcpy(mMatrix,m,sizeof(float)*sMatrixSize);

  return *this;  
}

template<int Dimension>
void VisusTransformation<Dimension>::setRotation(const VisusTransformation& t)
{
  for (int i=0;i<Dimension;i++) {
    for (int j=0;j<Dimension;j++) {
      mMatrix[i*(Dimension+1) + j] = t.mMatrix[i*(Dimension+1) + j];
    }
  }
}

template<int Dimension>
void VisusTransformation<Dimension>::setTranslation(const VisusTransformation& t)
{
  memcpy(mMatrix+Dimension*(Dimension+1),t.mMatrix+Dimension*(Dimension+1),
         sizeof(float)*Dimension);
}

template<int Dimension>
void VisusTransformation<Dimension>::toXML(XMLNode& parent) const
{
  XMLNode trans = parent.addChild(XML_TAG);
  trans.addAttribute("dim", Dimension);
  trans.addAttribute("rotateSpeed", mRotationSpeed);
  trans.addAttribute("translateSpeed", mTranslationSpeed);
  trans.addAttribute("scaleSpeed", mScalingSpeed);

  std::stringstream ss;
  for (int i=0;i<Dimension+1;i++) {
    for (int j=0;j<Dimension+1;j++) {
      ss << mMatrix[i*(Dimension+1) + j] << " ";
    }
  }
  trans.addText(ss.str().c_str());
}

template<int Dimension>
bool VisusTransformation<Dimension>::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusTransformation did not receive top node");
    vverbose("VisusTransformation did not receive top node", VISUS_XML_VERBOSE);
    return false;
  }

  const int dim = xmltoi(node.getAttribute("dim"), 0);
  if (dim != Dimension)
    return false;

  mRotationSpeed = xmltof(node.getAttribute("rotateSpeed"), sRotationSpeedDefault);
  mTranslationSpeed = xmltof(node.getAttribute("translateSpeed"), sTranslationSpeedDefault);
  mScalingSpeed = xmltof(node.getAttribute("scaleSpeed"), sScalingSpeedDefault);

  std::stringstream ss;
  ss << node.getText();

  for (int i=0;i<Dimension+1;i++) {
    for (int j=0;j<Dimension+1;j++) {
      ss >> mMatrix[i*(Dimension+1) + j];
    }
  }
  return true;
}


#endif

