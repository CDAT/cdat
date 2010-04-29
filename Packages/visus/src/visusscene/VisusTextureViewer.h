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


#ifndef VisusTextureViewer_H
#define VisusTextureViewer_H

#include "VisusGroup.h"
#include "VisusConsumer.h"
#include "VisusSmartPointer.h"
#include "VisusTexture.h"
#include "VisusTransformation3D.h"

class XMLNode;

class VisusTextureViewer;
typedef VisusSmartPointer<VisusTextureViewer> pVisusTextureViewer;

class VisusTextureViewer : public VisusGroup, public VisusConsumer
{
public:

  static pVisusTextureViewer instantiate();

  VisusTextureViewer();

  virtual ~VisusTextureViewer() {}

  //! Return the current orientation
  VisusSliceOrientation orientation() {return mOrientation;}
  
  //! Set the orientation
  void orientation(VisusSliceOrientation o);

  //! Position the P-I-P on screen
  void pipPosition(float x, float y);

  //! Set The Dimension Of P-I-P Box
  void pipSize(const float xSize, const float ySize);

  //! Set The Dimension of P-I-P Box As Scaling of Producer Bounding Box
  //  (aspect ratio same as image)
  void pipSize(const float scaling=1.0);

  //! Set The Color of the P-I-P Frame
  void pipColor(const float color[3]);
  void pipColor(const float red, const float green, const float blue);

  //! Define the rotation function as rotating the request 
  virtual void rotate(float x, float y) {rotateRequest(x,y);}
  
  //! Define the translation function as translating the request
  virtual void translate(float x, float y) {translateRequest(x,y);}

  //! Rotate the data request which should be a no-op
  void rotateRequest(float x, float y) {return;}

  //! Translate the request normal to the current slice
  void translateRequest(float x, float y);

  int connectInput(pVisusProducer producer);

  virtual void displayBoundingBox() const; 

  virtual void display(VisusTransformation3D model_view_3D = VisusTransformation3D(),
                       VisusTransformation2D model_view_2D = VisusTransformation2D(),
                       bool lock_graph = true);
protected:
  void toXMLLocalVariables(XMLNode& parent);
  bool fromXMLLocalVariables(XMLNode& node);

private:

  void setBBoxScreenCoords();
  void setExtent();

  void renderPIP(VisusTransformation3D& model_view_3D, VisusTransformation2D& model_view_2D);
  void renderFrame();
  void renderLines(VisusTransformation3D& model_view_3D, VisusTransformation2D& model_view_2D);

  bool linesIntersect(const double line1P1[], const double line1P2[],
                      const double line2P1[], const double line2P2[],
		                  double& x, double& y );

  //! Current orienation of the slice
  VisusSliceOrientation mOrientation;

  //! The data stored as texture
  VisusTexture mData;

  //! Color of P-I-P Frame
  GLdouble mBorderColor[3];

  //! Picture-In-Picture Box
  bool  mPIPSizeSet;
  float mXSize;
  float mYSize;
  float mPIPScaling;

  //! Bounding Box Of Texture Supplier
  GLdouble mLeftUpper[3], mLeftLower[3], mRightUpper[3], mRightLower[3];
  double mProjBoxPoints[8][3];
};



#endif
