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


#if WIN32
#include <windows.h>
#endif

#include <cstdlib>
#include <vector>
#include <math.h>
#include "glew.h"
#include "xmlParser.h"

#include "VisusDataRequest.h"
#include "VisusAssert.h"

const char* VisusDataRequest::XML_TAG = "VisusDataRequest";

VisusDataRequest::VisusDataRequest() : mMatrix(), mExtent(3,0), mStartStrides(3,1),
                                       mEndStrides(3,1), mNumResolutions(1), mTimeState(0), 
                                       mRequestType(VISUS_3D_REQUEST)
{  
}


VisusDataRequest::VisusDataRequest(const VisusDataRequest& request) : mTimeState(0)
{
  *this = request;
}

VisusDataRequest& VisusDataRequest::operator=(const VisusDataRequest& request)
{
  mMatrix = request.mMatrix;
  mDomainBBox = request.mDomainBBox;
  mExtent = request.extent();
  mStartStrides = request.mStartStrides;
  mEndStrides = request.mEndStrides;
  mNumResolutions = request.mNumResolutions;
  mTimeState = request.mTimeState;
  mRequestType = request.mRequestType;

  return *this;
}

bool VisusDataRequest::operator==(const VisusDataRequest& request)
{
  if (mMatrix != request.mMatrix)
    return false;

  if (mDomainBBox != request.mDomainBBox)
    return false;

  if (mExtent != request.mExtent)
    return false;
  
  if (mStartStrides != request.mStartStrides)
    return false;

  if (mEndStrides != request.mEndStrides)
    return false;
  
  //if (mTimeState != request.mTimeState)
  //return false;

  if (mRequestType != request.mRequestType)
    return false;

  return true;
}

VisusBoundingBox VisusDataRequest::queryRegion() const 
{
  VisusBoundingBox box;

  for (int i=0;i<3;i++) {
    box[i]   = -mExtent[i]/2;
    box[i+3] =  mExtent[i]/2;
  }
  box = box.translate(mMatrix[12],mMatrix[13],mMatrix[14]);

  box = box.rotateColumnMajor(mMatrix);
  //box = box.translate(mExtent[0]/2,mExtent[1]/2,mExtent[2]/2);

  return box;
}


std::vector<int> VisusDataRequest::strides(int i) const
{
  std::vector<int> strides(3,0);

  for (int k=0;k<3;k++) {
    strides[k] = mStartStrides[k] >> i;
    if (strides[k] < mEndStrides[k]) {
      vwarning("No such resolution. Defaulting to finest resolution requested.");
      strides[k] = mEndStrides[k];
    }
  }

  return strides;
}

int VisusDataRequest::setStrides(const std::vector<int>& start, const std::vector<int>& end)
{
  if (start.size() != end.size()) {
    vwarning("Start stride and end stride dimensions do not match. Setting of strides ignored");
    return 0;
  }

  for (unsigned int i=0;i<start.size();i++) {
    if (start[i] < end[i]) {
      vwarning("Start stride cannot be smaller end-stride. Setting of strides ignored");
      return 0;
    }

    if (end[i] <= 0) {
      vmessage("Strides must be larger or equal one. Setting strides to %d ignored",end[i]);
      return 0;
    }
  }

  mStartStrides = start;
  mEndStrides = end;

  //! For convinience compute the number of different resolutions
  mNumResolutions = determineNumResolutions();

  return 1;
}

int VisusDataRequest::timeState(const PvTimeState& time)
{
  mTimeState = time;
  
  return 1;
}

int VisusDataRequest::transformation(const VisusTransformation3D& matrix)
{
  mMatrix = matrix;
  return 1;
}


bool VisusDataRequest::valid()
{
  if (mExtent[0] < 0)
    return false;
  if (mExtent[1] < 0)
    return false;
  if (mExtent[2] < 0)
    return false;

  for (unsigned int i=0;i<mStartStrides.size();i++) {
    if (mStartStrides[i] < mEndStrides[i]) 
      return false;
  }
  
  return true;
}

void VisusDataRequest::drawBoundingBox() const
{
  glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);

  // Reset Color To White Since Request Doesn't Have BBox Color
  glColor3f(1.0,1.0,1.0);

  // The request stores the data relative to the center of the
  // bounding box 
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glMultMatrixf(mMatrix);

  glTranslatef(-0.5*mExtent[0],-0.5*mExtent[1],-0.5*mExtent[2]);

  glBegin(GL_QUADS);

  glVertex3f(0,0,0);
  glVertex3f(mExtent[0],0,0);
  glVertex3f(mExtent[0],mExtent[1],0);
  glVertex3f(0,mExtent[1],0);
  
  glVertex3f(0,0,mExtent[2]);
  glVertex3f(mExtent[0],0,mExtent[2]);
  glVertex3f(mExtent[0],mExtent[1],mExtent[2]);
  glVertex3f(0,mExtent[1],mExtent[2]);

  glEnd();

  glBegin(GL_LINES);

  glVertex3f(0,0,0);
  glVertex3f(0,0,mExtent[2]);
  
  glVertex3f(mExtent[0],0,0);
  glVertex3f(mExtent[0],0,mExtent[2]);
  
  glVertex3f(mExtent[0],mExtent[1],0);
  glVertex3f(mExtent[0],mExtent[1],mExtent[2]);
  
  glVertex3f(0,mExtent[1],0);
  glVertex3f(0,mExtent[1],mExtent[2]);
  
  glEnd();
  
  glPopMatrix();
}

VisusTransformation3D VisusDataRequest::translateToCenter() const
{
  return translationMatrix(-mExtent[0]/2,-mExtent[1]/2,-mExtent[2]/2);
}

int VisusDataRequest::determineNumResolutions() const 
{
  int factor = 1;
  int nr = 0;

  for (unsigned int i=0;i<mStartStrides.size();i++) {
    if (mStartStrides[i] / mEndStrides[i] > factor) 
      factor = mStartStrides[i] / mEndStrides[i];
  }

  while (factor > 0) {
    factor = factor >> 1;
    nr++;
  }

  return nr;
}

void VisusDataRequest::translate(const VisusTransformation3D& acc, 
                                 const VisusOpenGLState& state,float x, float y) 
{
  std::vector<float> trans;

  trans = mMatrix.translation(acc,state,x,y);

  //fprintf(stderr,"VisusDataRequest::translate  <%f, %f, %f>\n",trans[0],trans[1],trans[2]);
  //fprintf(stderr,"BoundingBox [%f,%f,%f] x [%f,%f,%f]\n",mDomainBBox[0],mDomainBBox[1],mDomainBBox[2],
  //        mDomainBBox[3],mDomainBBox[4],mDomainBBox[5]);

  mMatrix[12] += (mDomainBBox[3] - mDomainBBox[0])*trans[0];
  mMatrix[13] += (mDomainBBox[4] - mDomainBBox[1])*trans[1];
  mMatrix[14] += (mDomainBBox[5] - mDomainBBox[2])*trans[2];
}

void VisusDataRequest::shift(const VisusTransformation3D& acc, 
                             const VisusOpenGLState& state,float x, float y) 
{
  std::vector<float> trans;
  
  trans = mMatrix.freeTranslation(acc,state,x,y);

  //fprintf(stderr,"VisusDataRequest::translate  <%f, %f, %f>\n",trans[0],trans[1],trans[2]);
  //fprintf(stderr,"BoundingBox [%f,%f,%f] x [%f,%f,%f]\n",mDomainBBox[0],mDomainBBox[1],mDomainBBox[2],
  //        mDomainBBox[3],mDomainBBox[4],mDomainBBox[5]);

  if (mRequestType == VISUS_1D_REQUEST) {
    if (mExtent[0] > 0) {
      mMatrix[13] += (mDomainBBox[4] - mDomainBBox[1])*trans[1];
      mMatrix[14] += (mDomainBBox[5] - mDomainBBox[2])*trans[2];
    }
    else if (mExtent[1] > 0) {
      mMatrix[12] += (mDomainBBox[3] - mDomainBBox[0])*trans[0];
      mMatrix[14] += (mDomainBBox[5] - mDomainBBox[2])*trans[2];
    }
    else if (mExtent[2] > 0) {
      mMatrix[12] += (mDomainBBox[3] - mDomainBBox[0])*trans[0];
      mMatrix[13] += (mDomainBBox[4] - mDomainBBox[1])*trans[1];
    }
  }
  else if (mRequestType == VISUS_2D_REQUEST) {
    float delta = y*mMatrix.translationSpeed();


    mMatrix[12] += delta*(mDomainBBox[3] - mDomainBBox[0])*mMatrix[8];
    mMatrix[13] += delta*(mDomainBBox[4] - mDomainBBox[1])*mMatrix[9];
    mMatrix[14] += delta*(mDomainBBox[5] - mDomainBBox[2])*mMatrix[10];
   }
  else {
    mMatrix[12] += (mDomainBBox[3] - mDomainBBox[0])*trans[0];
    mMatrix[13] += (mDomainBBox[4] - mDomainBBox[1])*trans[1];
    mMatrix[14] += (mDomainBBox[5] - mDomainBBox[2])*trans[2];
  }

}

void VisusDataRequest::toXML(XMLNode& parent) const
{
  XMLNode dr = parent.addChild(XML_TAG);
  dr.addAttribute("type", mRequestType);
  dr.addAttribute("numResolutions", mNumResolutions);

  addChild(dr, "extent", mExtent);
  addChild(dr, "startStrides", mStartStrides);
  addChild(dr, "endStrides", mEndStrides);

  mTimeState.toXML(dr);
  mMatrix.toXML(dr);
}

bool VisusDataRequest::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusDataRequest did not receive its top level tag");
    vverbose("VisusDataRequest did not receive its top level tag", VISUS_XML_VERBOSE);
    return false;
  }

  mRequestType = (VisusRequestType)xmltoi(node.getAttribute("type"), mRequestType);
  mNumResolutions = xmltoi(node.getAttribute("numResolutions"), mNumResolutions);

  getChild(node, "extent", mExtent);
  getChild(node, "startStrides", mStartStrides);
  getChild(node, "endStrides", mEndStrides);

  XMLNode child = node.getChildNode(PvTimeState::XML_TAG);
  if (! mTimeState.fromXML(child))
    return false;

  child = node.getChildNode(VisusTransformation<3>::XML_TAG);
  if (! mMatrix.fromXML(child))
    return false;

  return true;
}
