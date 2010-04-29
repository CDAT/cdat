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

#include <iostream>

#include "xmlParser.h"

#include "VisusAssert.h"
#include "VisusText.h"

#include "glew.h"
#include <string.h>
#include <math.h>
  
const char* VisusText::XML_TAG = "VisusText";

const float VisusText::DEFAULT_CANVAS_HEIGHT = 300;
const float VisusText::sLeftRightMatrix[16] = {1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1};
const float VisusText::sRightLeftMatrix[16] = {-1,0,0,0,0,-1,0,0,0,0,1,0,0,0,0,1};
const float VisusText::sTopBottomMatrix[16] = {0,-1,0,0,1,0,0,0,0,0,1,0,0,0,0,1};
const float VisusText::sBottomTopMatrix[16] = {0,1,0,0,-1,0,0,0,0,0,1,0,0,0,0,1};

VisusText::VisusText(const char *st)
{
  if (st == NULL) {
    mText = new char[1];
    mText[0] = '\0';
    mBufferSize = 1;
  }
  else {
    mBufferSize = strlen(st)+1;
    mText = new char[mBufferSize];
    strcpy(mText,st);
  }
  mOrientation = VISUS_LEFT_RIGHT;
}

VisusText::VisusText(const VisusText& text) : mText(NULL)
{
  //if (mText != NULL)
  //  delete mText;

  mBufferSize = text.mBufferSize;
  mText = new char[mBufferSize];
  strcpy(mText,text.text());
  
  mOrientation = text.orientation();
  mMatrix = text.mMatrix;
  mFont = text.mFont;
}

VisusText::~VisusText() 
{
  if (mText != NULL)
    delete[] mText;
}


void VisusText::text(const char *st)
{
  if (st == NULL) {
    mText[0] = '\0';
  }
  else {
    if (mBufferSize <= (int)strlen(st)) {
      mBufferSize = strlen(st)+1;
      if (mText != NULL)
        delete[] mText;
      mText = new char[mBufferSize];
    }

    strcpy(mText,st);
  }
}

const char *VisusText::text() const
{
  return mText;
}

void VisusText::font(const VisusFont& font)
{
	mFont = font;
}

void VisusText::orientation(VISUS_TEXT_ORIENTATION orient) 
{ 
  mOrientation = orient; 
}

VisusBoundingBox VisusText::bbox(const char *st)
{
  VisusBoundingBox bb;
  float tmp[6];

  // Make sure we do the same as render so size is consistent
  GLint viewport[4];
  glGetIntegerv(GL_VIEWPORT,viewport);
  mFont.fontRenderScale(viewport[3]/DEFAULT_CANVAS_HEIGHT);

  // Imaging a canvas of height DEFAULT_CANVAS_HEIGHT
  //mFont.fontRenderScale(1);

  // get the original box in pixels
  if (st == NULL)
    mFont.bbox(mText,tmp);
  else
    mFont.bbox(st,tmp);
  
  bb.set(tmp);

  // If we are a polygonal font we potentially must rotate
  if ((mFont.fontStyle() != VISUS_BITMAP) 
      && (mFont.fontStyle() != VISUS_PIXMAP)) {
 
    // rotate the box according to our rotation and re-compue an
    // axis-aligned box
    switch (mOrientation) {
    case VISUS_LEFT_RIGHT:
      bb = bb.rotateColumnMajor(sLeftRightMatrix);
      break;
    case VISUS_RIGHT_LEFT:
      bb = bb.rotateColumnMajor(sRightLeftMatrix);
      break;
    case VISUS_TOP_BOTTOM:
      bb = bb.rotateColumnMajor(sTopBottomMatrix);
      break;
    case VISUS_BOTTOM_TOP:
      bb = bb.rotateColumnMajor(sBottomTopMatrix);
      break;
    }
  }

  // Now scale the bounding box to [-1,1]^2
  for (int i=0;i<6;i++)
    bb[i] /= 0.5*DEFAULT_CANVAS_HEIGHT*mFont.fontRenderScale();

  return bb;
}

void VisusText::render(int screenHeight, const char *st)
{
  //std::cout << "VisusText::render" << std::endl;

  VisusBoundingBox bb; // the current bounding box
  float scale; // scale factor between bitmapped and polygonal fonts
  GLdouble id[16] = {1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1};
  GLdouble projection[16];
  GLint viewport[4];
  GLdouble screenCoor[6];
  int pushed=0;

  // get the current viewport
  glGetIntegerv(GL_VIEWPORT,viewport);

  // update the font size according to the given canvas height
  mFont.fontRenderScale(viewport[3]/DEFAULT_CANVAS_HEIGHT);

  if ((mFont.fontStyle() != VISUS_BITMAP) 
      && (mFont.fontStyle() != VISUS_PIXMAP)) 
  {

    // If the font is not a bitmapped font 
    // We figure out the correct scaling factor between the bitmaped and
    // polygonal fonts. This is done by projection the unit y-vector to the screen
    // and determining its projected height

    // Get the projection matrix 
    glGetDoublev(GL_PROJECTION_MATRIX,projection);
    
    // unproject the unit y-vector assuming identity for the modelview
    gluProject(0,0,0,id,projection,viewport,screenCoor,screenCoor+1,screenCoor+2);
    gluProject(0,1,0,id,projection,viewport,screenCoor+3,screenCoor+4,screenCoor+5);
    
    // calculate the relative scaling 
    scale = 1 / fabs(screenCoor[4] - screenCoor[1]); 
    
    // get the current bounding box
    bb = bbox(st);

    //fprintf(stderr,"VisusText::render() BBox %f %f %f %f\n",bb[0],bb[1],bb[3],bb[4]);
    
    glPushMatrix();
    pushed = 1;

    // scale the polygonal fonts to the bitmapped ones
    glScalef(scale,scale,scale);
    
    // For all the non-bitmapped fonts
    // we want to process the scaling and rotation. Remember that
    // those are specified in reverse

    // Return my bounding box to the original position
    glTranslatef((bb[0]+bb[3])/2.0,(bb[1]+bb[4])/2.0,(bb[2]+bb[5])/2.0);

    //Rotate
    switch (mOrientation) {
    case VISUS_LEFT_RIGHT:
      glMultMatrixf(sLeftRightMatrix);
      break;
    case VISUS_RIGHT_LEFT:
      glMultMatrixf(sRightLeftMatrix);
      break;
    case VISUS_TOP_BOTTOM:
      glMultMatrixf(sTopBottomMatrix);
      break;
    case VISUS_BOTTOM_TOP:
      glMultMatrixf(sBottomTopMatrix);
      break;
    }

    // Translate the center of the bbox to the origin
    glTranslatef(-(bb[0]+bb[3])/2.0,-(bb[1]+bb[4])/2.0,-(bb[2]+bb[5])/2.0);
  }

  glDisable(GL_DEPTH_TEST);
  if (st == NULL)
    mFont.render(mText);
  else
    mFont.render(st);
  glEnable(GL_DEPTH_TEST);

  if (pushed)
    glPopMatrix();
}


void VisusText::toXML(XMLNode& parent) const
{ 
  XMLNode node = parent.addChild(XML_TAG);
  node.addText(text());
  mFont.toXML(node);
}

bool VisusText::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    std::stringstream ss;
    ss << "VisusText did not receive top level node. received (" << node.getName() << ")";
    vwarning(ss.str().c_str());
    vverbose(ss.str().c_str(), VISUS_XML_VERBOSE);
    return false;
  }
  
  text(node.getText());

  XMLNode n = node.getChildNode(0);
  if (! mFont.fromXML(n)) {
    vwarning("Failed to retrieve font for VisusText");
    return false;
  }

  return true;
}

