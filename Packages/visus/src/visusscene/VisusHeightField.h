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


#ifndef VISUSHEIGHTFIELD_H
#define VISUSHEIGHTFIELD_H

#include "VisusGroup.h"
#include "VisusConsumer.h"
#include "VisusSmartPointer.h"
#include "VisusTexture.h"
#include "VisusTransformation3D.h"

enum HFInputData {
  HF_HEIGHT=0,
  HF_COLOR=1
};

class VisusHeightField;
typedef VisusSmartPointer<VisusHeightField> pVisusHeightField;

class VisusHeightField : public VisusGroup, public VisusConsumer
{
public:

  static pVisusHeightField instantiate();

  VisusHeightField();

  virtual ~VisusHeightField() {}

  //! Return the current orientation
  VisusSliceOrientation orientation() {return mOrientation;}
  
  //! Set the orientation
  void orientation(VisusSliceOrientation o);

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
  
  //! Connect to the producer which provides the height data
  int connectHeight(pVisusProducer producer);

  //! Connect to the producer which provides the color data
  int connectColor(pVisusProducer producer);

  virtual void displayBoundingBox() const; 


protected:
  //! Store Attributes into XML stream
  void toXMLLocalVariables(XMLNode& parent);

  //! Load instance with XML attributes
  bool fromXMLLocalVariables(XMLNode& node);

  virtual void display3D(VisusTransformation3D model_view_3D = VisusTransformation3D());
private:

  template<typename T> void drawValues(T* data);  


  //! Current orienation of the slice
  VisusSliceOrientation mOrientation;

  //! The height data
  VisusBlockData mHeightData;

  //! The color data stored as texture
  VisusTexture mColorData;
};



#endif
