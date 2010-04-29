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


#ifndef VISUSINDEXEDDISPLAY_H
#define VISUSINDEXEDDISPLAY_H

#include "VisusGroup.h"
#include "VisusConsumer.h"
#include "VisusSmartPointer.h"
#include "VisusMeshData.h"


class VisusIndexedDisplay;
typedef VisusSmartPointer<VisusIndexedDisplay> pVisusIndexedDisplay;

class VisusIndexedDisplay : public VisusGroup, public VisusConsumer
{
public:

  static pVisusIndexedDisplay instantiate();

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  

  //! Default constructor
  VisusIndexedDisplay();

  //! Destructor
  virtual ~VisusIndexedDisplay();

  //! Return an info string identifying the node
  virtual std::string infoString() const {return std::string("Indexed data display");}

  /***************************************************************
   ******          Access Functions                      *********
   **************************************************************/  

  //! Return the current vertex coordinate where the normal starts
  int normalIndex() const {return mNormalIndex;}
  
  //! Return the current coordinate used for coloring
  int colorIndex() const {return mColorIndex;}

  //! Return the glPolyonMode
  int polygonMode() const {return mPolygonMode;}

  //! Return the current lighting flag
  bool enableLighting() {return mEnableLighting;}

  //! Set the normal index
  void normalIndex(int index) {mNormalIndex = index;}
  
  //! Set the color index
  void colorIndex(int index) {mColorIndex = index;}

  //! Set the glPolygonMode
  void polygonMode(int mode) {mPolygonMode = mode;}
  
  //! Set the lighting flag
  void enableLighting(bool lighting) {mEnableLighting = lighting;}

  //! Set the geometry primitive
  void geometryPrimitive(int primitive) {mGeometryPrimitive = primitive;}

  //! Freeze all transformations influencing the drawing
  virtual void freeze();

  int connectInput(pVisusProducer producer);

  int loadData(VisusData* data, int input);

private:
  
  //! The mesh that should be displayed
  VisusIndexedData* const mMesh;

  //! Index where the normal component starts
  int mNormalIndex;

  //! Which vertex coordinate should be used as color
  int mColorIndex;

  //! glPolygonMode
  int mPolygonMode;

  //! The OpenGL primitive used for drawing
  int mGeometryPrimitive;
  
  //! Flag to indicate whether to use lighting when drawing
  bool mEnableLighting;

  //! Frozen data matrix 
  VisusTransformation3D mFrozenDataTransformation;

  virtual void display3D(VisusTransformation3D model_view_3D = VisusTransformation3D());
 
  //! Render a colormapped surface/line
  void renderColoredSmooth(void (VisusIndexedDisplay::*vertexCall)(int));
  
  //! Render a colormapped flat shaded surface/line
  void renderColoredFlat(void (VisusIndexedDisplay::*vertexCall)(int));
  
  //! Render a solid color smooth surface/line
  void renderSmooth(void (VisusIndexedDisplay::*vertexCall)(int));
  
  //! Render a solid flat shaded surface/line
  void renderFlat(void (VisusIndexedDisplay::*vertexCall)(int));
  
  //! Wrapper function for to draw 2D vertex
  void vertex2f(int i) {glVertex2fv(mMesh->vertexAddress(i));}
  
  //! Wrapper function for to draw 3D vertex
  void vertex3f(int i) {glVertex3fv(mMesh->vertexAddress(i));}
  
};



#endif
