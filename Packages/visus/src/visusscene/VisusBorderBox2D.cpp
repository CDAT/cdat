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

#include "xmlParser.h"

#include "VisusBorderBox2D.h"
#include "VisusBoundingBox.h"

#include "glew.h"
#include <math.h>

#include <iostream>

/***************************************************************
 ******       Public Default Values                    *********
 **************************************************************/ 

const float         VisusBorderBox2D::DEFAULT_LENGTH            = 0.9;
const float         VisusBorderBox2D::DEFAULT_WIDTH             = 0.05;
const BBOrientation VisusBorderBox2D::DEFAULT_ORIENTATION       = BB_HORIZONTAL;
 
/***************************************************************
 ******       Private Default Values                   *********
 **************************************************************/ 
const float VisusBorderBox2D::LEFT_RIGHT_MATRIX[16] = {1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1};
const float VisusBorderBox2D::BOTTOM_TOP_MATRIX[16] = {0,1,0,0,-1,0,0,0,0,0,1,0,0,0,0,1};
const float VisusBorderBox2D::TOP_BOTTOM_MATRIX[16] = {0,-1,0,0,1,0,0,0,0,0,1,0,0,0,0,1};  
const float VisusBorderBox2D::RIGHT_LEFT_MATRIX[16] = {-1,0,0,0,0,-1,0,0,0,0,1,0,0,0,0,1};
  

VisusBorderBox2D::VisusBorderBox2D(const VisusNodeType nodeType):VisusGroup(nodeType)
{
  declareParameter<VisusSharedTransformation2D>();
  declareParameter<VisusSharedColor>();

  this->mBoundingBoxMode = VISUS_2D_RENDER;

  VisusColor color;
  getValue(color);

  mBackgroundColor = VisusColor(0,0,0,0);

  VisusFont font;
  font.fontSize(12);

  mTitle.text("");
  mTitle.font(font);

  mTitleAlignment = AXIS_LEFT_ALIGN;

  mTickLineType = BB_NONE;
}  

void VisusBorderBox2D::position(float x, float y)
{
  VisusTransformation2D local;
  
  getValue(local);
  local[6] = x;
  local[7] = y;
  setValue(local);  
}

VisusBorderAxis VisusBorderBox2D::axis(const BBAxis axis)
{
  if (axis == BB_X_AXIS)
    return mXAxis;
  
  return mYAxis; 
}

void VisusBorderBox2D::axis(const BBAxis axis, const VisusBorderAxis& config)
{
  if (axis == BB_X_AXIS)
    mXAxis = config;
  else
    mYAxis = config; 
}

void VisusBorderBox2D::backgroundColor(const VisusColor& color)
{
  mBackgroundColor = color;
}

void VisusBorderBox2D::titleText(const char* title)
{
  mTitle.text(title);
} 

void VisusBorderBox2D::titleFont(const VisusFont& font)
{
  mTitle.font(font);
}

void VisusBorderBox2D::titleAlignment(AXISAlignment alignment)
{
  mTitleAlignment = alignment;
}


void VisusBorderBox2D::render2D(VisusTransformation2D& model_view_2D)
{
  //std::cout << "BB2D render2D" << std::endl;

  // Position Appropriately
  VisusTransformation2D local;
  getValue(local);

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();

  model_view_2D *= local;
  glTranslatef(model_view_2D[6],model_view_2D[7],0);

  // Draw Bounding Box If Selected
  if (mDrawBoundingBox)
    displayBoundingBox();

  GLint viewport[4];  // x, y, width, height
  int canvasWidth, canvasHeight;

  // Get the current viewport
  glGetIntegerv(GL_VIEWPORT,viewport);
  canvasWidth = viewport[2];
  canvasHeight= viewport[3];
  
  glDisable(GL_LIGHTING);

  // We always draw the geometry horizontally. If necessary we rotate
  if (mOrientation == BB_VERTICAL)
    glMultMatrixf(BOTTOM_TOP_MATRIX);

  glTranslatef(0,0,-1);
  // Ensure Ticks Same Color As Border
  VisusColor color;
  getValue(color);
  mXAxis.tickColor(color);
  mYAxis.tickColor(color);

  // 1) Draw the tick marks
  mXAxis.renderTickMarks(mWidth, mLength, BB_X_AXIS, mTickLineType);
  mYAxis.renderTickMarks(mWidth, mLength, BB_Y_AXIS, mTickLineType);

  glTranslatef(0,0,+0.5);
  if (mTickLineType == BB_NONE)
    clearInsideBox();

  glTranslatef(0,0,+0.5);
  // 2) Draw the border of the box
  renderBorder(mXAxis.mWidth, mXAxis.mLength);

  // 3) Draw inside of the box
  if (! renderInsideBox(mXAxis.mWidth, mXAxis.mLength))
    vwarning("Error occurred rendering inside the VisusBorderBox2D");
  vglerror();

  // 4) Draw labels
  mXAxis.renderLabels(canvasWidth, canvasHeight);
  mYAxis.renderLabels(canvasWidth, canvasHeight);

  // 5) Draw legend
  mXAxis.renderLegend(canvasWidth, canvasHeight);
  mYAxis.renderLegend(canvasWidth, canvasHeight);

  // 6) Draw title
  renderTitle(canvasWidth, canvasHeight, mXAxis.mWidth, mXAxis.mLength);

  // Reverse the rotation
  if (mOrientation == BB_VERTICAL)
    glMultMatrixf(TOP_BOTTOM_MATRIX);
    
  glDisable(GL_LIGHTING);

  glPopMatrix();
  vglerror();
}

void VisusBorderBox2D::renderTitle(const int canvasWidth, const int canvasHeight,
                                   const int boxWidth, const int boxLength)
{
  if (strlen(mTitle.text())==0)
    return;

  VisusBoundingBox bbox;

  float trans[2] = { 0, 0};
  
  bbox = mTitle.bbox();

  switch (mOrientation)
  {
  case BB_VERTICAL:
    {
      float dist = mXAxis.getDrawBox().rightUpper()[1] - mXAxis.getDrawBox().leftLower()[1];
      trans[0] = mYAxis.getDrawBox().rightUpper()[1];
      trans[1] = mXAxis.getDrawBox().leftLower()[0] + mXAxis.legendOffset() + dist;
    }
  default:
    {
      trans[1] = mYAxis.getDrawBox().rightUpper()[1] + mXAxis.legendOffset();
 
      // Align X appropriately with border
      switch (mTitleAlignment) {
      case AXIS_LEFT_ALIGN:
        trans[0] = mXAxis.getDrawBox().leftLower()[0];
        break;
      case AXIS_RIGHT_ALIGN:
	trans[0] = mXAxis.getDrawBox().rightUpper()[0] - (bbox[3] - bbox[0]);
        break;
      case AXIS_CENTER_ALIGN:
	trans[0]= (mXAxis.getDrawBox().rightUpper()[0] + mXAxis.getDrawBox().leftLower()[0])/2.0 - (bbox[3] - bbox[0])/2.0;
        break;
      } 
    }
  }
  
  // Translate
  glTranslatef(trans[0],trans[1],0);
    
  //Draw Text
  mTitle.render(canvasHeight);

  // Reverse Translation
  trans[0] *= -1;
  trans[1] *= -1;
  glTranslatef(trans[0],trans[1],0);
}

void VisusBorderBox2D::renderBorder(const float boxWidth, const float boxLength)
{
  VisusColor color;
  getValue(color);

  float borderExtend = 0.002;
  
  color.glColor();
  glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);

  glBegin(GL_QUADS);

  glVertex2f(0-borderExtend,boxWidth+borderExtend);
  glVertex2f(0-borderExtend,0-borderExtend);
  glVertex2f(boxLength+borderExtend,0-borderExtend);
  glVertex2f(boxLength+borderExtend,boxWidth+borderExtend);
  
  glEnd();

  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
}


void VisusBorderBox2D::clearInsideBox()
{
  mBackgroundColor.glColor();
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

  glBegin(GL_QUADS);

  glVertex2f(0,mWidth);
  glVertex2f(0,0);
  glVertex2f(mLength,0);
  glVertex2f(mLength,mWidth);
	  
  glEnd();
}


void VisusBorderBox2D::toXMLLocalVariables(XMLNode& parent)
{
  parent.addAttribute("length", mLength);
  parent.addAttribute("width", mWidth);
  parent.addAttribute("orientation", mOrientation);

  XMLNode node = parent.addChild("xaxis");
  mXAxis.toXML(node);

  node = parent.addChild("yaxis");
  mYAxis.toXML(node);

  node = parent.addChild("borderColor");
  mBorderColor.toXML(node);

  node = parent.addChild("backgroundColor");
  mBackgroundColor.toXML(node);
}


bool VisusBorderBox2D::fromXMLLocalVariables(XMLNode& node)
{
  if (strcmp(VisusGroup::XML_TAG,node.getName())) 
  {
    std::stringstream ss;
    ss << "VisusBorderBox2D did not receive top level node. received (" << node.getName() <<")\n";
    vwarning(ss.str().c_str());
    vverbose(ss.str().c_str(), 100);
    return false;
  }
  mLength = xmltof(node.getAttribute("length"), mLength);
  mWidth  = xmltof(node.getAttribute("width"), mWidth);
  mOrientation = (BBOrientation) xmltoi(node.getAttribute("orientation"), mOrientation);

  XMLNode child = node.getChildNode("xaxis");
  XMLNode n  = child.getChildNode(0);
  if (! mXAxis.fromXML(n))
    return false;

  child = node.getChildNode("yaxis");
  n  = child.getChildNode(0);
  if (! mYAxis.fromXML(n))
    return false;

  child = node.getChildNode("borderColor");
  n  = child.getChildNode(0);
   if (! mBorderColor.fromXML(n))
    return false;

  child = node.getChildNode("backgroundColor");
  n  = child.getChildNode(0);
    if (! mBackgroundColor.fromXML(n))
    return false;

  return true;
}
