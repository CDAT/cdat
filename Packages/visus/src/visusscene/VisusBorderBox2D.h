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


#ifndef VISUSBORDERBOX2D_H
#define VISUSBORDERBOX2D_H

#include <vector>
#include <string>

#include "VisusColor.h"
#include "VisusGroup.h"
#include "VisusBorderAxis.h"
#include "VisusSmartPointer.h"
#include "VisusText.h"


class VisusBoundingBox;

//! Type to indicate the orientation 
enum BBOrientation 
{
  BB_HORIZONTAL = 0,  //! Draw normal
  BB_VERTICAL = 1,    //! Flip X and Y
};

class VisusBorderBox2D : public VisusGroup
{
 public:
	 
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  

  //! Destructor
  virtual ~VisusBorderBox2D() {};

  /***************************************************************
   ******       Set/Get Functions                        *********
   **************************************************************/  

  //! Set the orientation of the border box
  void orientation(BBOrientation o) {mOrientation = o;}

  //! Set the x,y location of the border box within the parent bounding box
  void position(float x, float y);

  //! Get Attributes of Axis
  VisusBorderAxis axis(const BBAxis axis);

  //! Set Attributes of Axis
  void axis(const BBAxis axis, const VisusBorderAxis& config);

  //! Prevent the color bar from rotating
  virtual void rotate(float x, float y) {}

  //! The translation for the color is the 2D standard translation
  virtual void translate(float x, float y) {translate2D(x,y);}
  
  //! Set the background color of border box
  void backgroundColor(const VisusColor& color);

  //! Set whether to draw tick marks through graph
  void tickLineType(const BBTickLine type) { mTickLineType = type; }

  //! Set title of graph
  void titleText(const char* title);

  //! Set font of title of graph
  void titleFont(const VisusFont& font);

  //! Set alignment of title of graph
  void titleAlignment(AXISAlignment alignment);

  /***************************************************************
   ******       Private Variables And Functions          *********
   **************************************************************/  
protected:

  //! Store Attributes into XML stream
  virtual void toXMLLocalVariables(XMLNode& parent);

  //! Load instance with XML attributes
  virtual bool fromXMLLocalVariables(XMLNode& node);

  //! Initialize the base class
  VisusBorderBox2D(const VisusNodeType nodeType);

  //! Draw everything in 2D
  void render2D(VisusTransformation2D& model_view_2D);

  //! Derivations should implement this function to draw within 2D box
  virtual bool renderInsideBox(const float boxWidth, const float boxLength)=0;

  //! Start the inside of box with a "black box"
  void clearInsideBox();
  
 /***************************************************************
  ******       Axis Configuration                       *********
  **************************************************************/  
  VisusBorderAxis mXAxis;
  VisusBorderAxis mYAxis;

  //! Length relative to the [-1,1]^2 canvas 
  float mLength;

  //! Width relative to the [-1,1]^2 canvas 
  float mWidth;

  //! Global orientation
  BBOrientation mOrientation;

  //! Title
  VisusText mTitle;

  AXISAlignment mTitleAlignment;

  BBTickLine mTickLineType;

private:

  VisusBorderBox2D() {} // Private so no one can construct
  
  void renderTitle(const int canvasWidth, const int canvasHeight,
                   const int boxWidth, const int boxLength);

  void renderBorder(const float boxWidth, const float boxLength);

  void renderTickMarks(const VisusBorderAxis& config, const BBAxis axis,
                       float& width, float& length,
                       VisusBoundingBox &bb);

  void renderLabels(const int canvasWidth, const int canvasHeight, 
                    VisusBorderAxis& config, const BBAxis axis,
                    VisusBoundingBox &bb);

  void renderLegend(const int canvasWidth, const int canvasHeight, 
                    VisusBorderAxis& config, const BBAxis axis,
                    VisusBoundingBox &bb);
 
  //! Color of the border as well as the tick marks
  VisusColor mBorderColor;

  //! Color used to clear out inside of box aka background color
  VisusColor mBackgroundColor;

  /***************************************************************
   ******       Default Values                           *********
   **************************************************************/  

  static const float DEFAULT_LENGTH;
  static const float DEFAULT_WIDTH;
  static const BBOrientation DEFAULT_ORIENTATION;

  /***************************************************************
   ******       Preset OpenGL Tranformations              *********
   **************************************************************/    
  
  //! OpenGl matrix to draw text left to right (identity)
  static const float LEFT_RIGHT_MATRIX[16];
  
  //! OpenGl matrix to draw text bottom to top 
  static const float BOTTOM_TOP_MATRIX[16];
  
  //! OpenGl matrix to draw text top to bottom  
  static const float TOP_BOTTOM_MATRIX[16];

  //! OpenGl matrix to draw text right to left (upside-down)
  static const float RIGHT_LEFT_MATRIX[16];
   
};

#endif
