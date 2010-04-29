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


#ifndef VISUSMESHDISPLAY_H
#define VISUSMESHDISPLAY_H

#include "VisusGroup.h"
#include "VisusConsumer.h"
#include "VisusSmartPointer.h"
#include "VisusMeshData.h"

class VisusTexture;

struct XMLNode;

class VisusMeshDisplay;
typedef VisusSmartPointer<VisusMeshDisplay> pVisusMeshDisplay;

class VisusMeshDisplay : public VisusGroup, public VisusConsumer
{
public:

  static pVisusMeshDisplay instantiate();

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  

  //! Default constructor
  VisusMeshDisplay();

  //! Destructor
  virtual ~VisusMeshDisplay();

  /***************************************************************
   ******          Access Functions                      *********
   **************************************************************/  

  //! Return the current vertex coordinate where the normal starts
  int normalIndex() const {return mNormalIndex;}
  
  //! Return the current coordinate used for coloring
  int colorIndex() const {return mColorIndex;}

  //! Return the glPolyonMode
  int polygonMode() const {return mPolygonMode;}

  //! Set the normal index
  void normalIndex(int index) {mNormalIndex = index;}
  
  //! Set the color index
  void colorIndex(int index) {mColorIndex = index;}

  //! Set the glPolygonMode
  void polygonMode(int mode) {mPolygonMode = mode;}

  //! Freeze all transformations influencing the drawing
  virtual void freeze();
  
  int connectInput(pVisusProducer producer);

  int loadData(VisusData* data, int input);
  
  void minValue(const float minValue) { mMinValue = minValue; }
  void maxValue(const float maxValue) { mMaxValue = maxValue; }

protected:

  void toXMLLocalVariables(XMLNode& parent);
  bool fromXMLLocalVariables(XMLNode& parent);

  virtual void display3D(VisusTransformation3D model_view_3D = VisusTransformation3D());
 
private:
  
  static const float DEFAULT_MIN;
  static const float DEFAULT_MAX;
  static const int   DEFAULT_INDEX;
  static const int   DEFAULT_MODE;

  //! The mesh that should be displayed
  VisusMeshData mMesh;

  //! Index where the normal component starts
  int mNormalIndex;

  //! Which vertex coordinate should be used as color
  int mColorIndex;

  //! glPolygonMode
  int mPolygonMode;
  
  //! Min value for ColorMap when rendering in color
  float mMinValue;
  
  //! Max value for ColorMap when rending in color
  float mMaxValue;

  float mValueRange;
  
   //! Frozen data matrix 
  VisusTransformation3D mFrozenDataTransformation;

 //! Render a colormapped smooth surface
  void renderColoredSmooth();
  
  //! Render a colormapped flat shaded surface
  void renderColoredFlat();
  
  //! Render a solid color smooth surface
  void renderSmooth();
  
  //! Render a solid flat shaded surface
  void renderFlat();
  
  //! Compute Min and Max Value Based on Current Color Values
  void computeMinMax();  
  
  //! Set Color Based on ColorMap and Color Value
  void setColor(VisusColorMap& colorMap, int colorValue);
};



#endif
