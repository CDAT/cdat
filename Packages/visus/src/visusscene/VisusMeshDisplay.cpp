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


#ifdef WIN32
#include <windows.h>
#endif

#include "glew.h" 
#include "xmlParser.h"

#include "VisusMeshDisplay.h"
#include "VisusSharedColorMap.h"
#include "VisusSharedColor.h"
#include "VisusBoundingBox.h"
#include "VisusStdInt.h"

const float VisusMeshDisplay::DEFAULT_MIN = 1.0e+10;
const float VisusMeshDisplay::DEFAULT_MAX = 1.0e+10;
const int   VisusMeshDisplay::DEFAULT_INDEX = -1;
const int   VisusMeshDisplay::DEFAULT_MODE = GL_FILL;


pVisusMeshDisplay VisusMeshDisplay::instantiate()
{
  return gObjectFactory.instantiate<VisusMeshDisplay>();
}


VisusMeshDisplay::VisusMeshDisplay() : VisusGroup(VISUS_MESH_NODE), VisusConsumer(1), mMesh(6,3)
{
  declareParameter<VisusSharedColorMap>();
  declareParameter<VisusSharedColor>();
  
  mMinValue = DEFAULT_MIN;
  mMaxValue = DEFAULT_MAX;
  
  // Initially we cannot assume we are given a normal
  mNormalIndex = DEFAULT_INDEX;
  
  // Nor can we assume we have a color index
  mColorIndex = DEFAULT_INDEX;

  mPolygonMode = DEFAULT_MODE;
  mSinks[0] = &mMesh;
}


VisusMeshDisplay::~VisusMeshDisplay()
{
}

void VisusMeshDisplay::freeze()
{
  mFrozenDataTransformation = mMesh.matrix();

  VisusGroup::freeze();
}


void VisusMeshDisplay::display3D(VisusTransformation3D model_view_3D)
{
  VisusTransformation3D local;
  VisusColor color;

  synchronize();


  getValue(local);
  getValue(color);

  if (!frozen())
    model_view_3D *= local;
  else
    model_view_3D = mFrozen3D;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadMatrixf(model_view_3D);
  
  if (!frozen())
    glMultMatrixf(mMesh.matrix());
  else
    glMultMatrixf(mFrozenDataTransformation);

  if (mDrawBoundingBox) {
    //fprintf(stderr,"draw bounding box\n");
    displayBoundingBox();
  }

  glPolygonMode(GL_FRONT_AND_BACK,mPolygonMode);

  //float c[4] = {1,1,1,1};
  //glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, c);
  glColor3f(1,1,1);
  
  if ((mColorIndex >= 0) && (mNormalIndex >= 0)) {
    renderColoredSmooth();
  }
  else if ((mColorIndex >= 0) && (mNormalIndex < 0))
    renderColoredFlat();
  else if ((mColorIndex < 0) && (mNormalIndex >= 0)) {
    getValue(color);
    color.glColor();
    renderSmooth();
  }
  else {
    getValue(color);
    color.glColor();
    renderFlat();
  }
  


  //glEnable(GL_TEXTURE_2D);  // This breaks anything else that uses colormap on windows if enabled
  //glDisable(GL_COLOR_MATERIAL);
  vglerror();

  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display(model_view_3D);
 
  glPopMatrix();  
}

int VisusMeshDisplay::connectInput(pVisusProducer producer)
{
  if (connect(0,producer,&mMesh) == 0) {
    vwarning("Could not connect producer.");
    return 0;
  }

  return 1;
}

int VisusMeshDisplay::loadData(VisusData* data, int input)
{
  if (VisusConsumer::loadData(data,input) != 0) {
    setValue(mMesh.boundingBox());
    return 1;
  }

  return 0;
}

void VisusMeshDisplay::computeMinMax()
{
  for (uint32_t i=0;i<mMesh.nrOfElements();i++) 
  {
    std::vector<VisusMeshData::IndexDataType>& element = mMesh.element(i);
	for (std::vector<VisusMeshData::IndexDataType>::iterator it=element.begin();it!=element.end();it++) 
	{ 
	  float colorValue = mMesh.vertex(*it)[mColorIndex];
	  if (colorValue < mMinValue)
	    mMinValue = colorValue;
	  if (colorValue > mMaxValue)
	    mMaxValue = colorValue;
	}
  }
}

void VisusMeshDisplay::setColor(VisusColorMap& colorMap, int colorValue)
{
  unsigned char ucolor[4];
  float color[4];
  
  colorMap.getColor((colorValue - mMinValue) / mValueRange, ucolor);
  color[0] = ucolor[0] / 255.0;
  color[1] = ucolor[1] / 255.0;
  color[2] = ucolor[2] / 255.0;
  
  glColor3fv(color);	
}

void VisusMeshDisplay::renderColoredSmooth()
{
  std::vector<VisusMeshData::IndexDataType>::iterator it;

  pVisusSharedColorMap colorMap = sharedValue<VisusSharedColorMap>();
  if (colorMap->lockToRead() == 0) {
    vwarning("Could not lock color map for reading. Rendering aborted.");
    return;
  }

  glShadeModel(GL_SMOOTH);
  //fprintf(stderr,"VisusMeshDisplay::renderColoredSmooth()\n");
  // Find Min / Max for color map if not hand-set
  if (mMinValue > mMaxValue) 
    computeMinMax();
  mValueRange = mMaxValue - mMinValue;
  
  // Get Local Colormap
  switch (mMesh.elementDim()) {
  case 3:
    glBegin(GL_TRIANGLES);
    break;
  case 4:
    glBegin(GL_QUADS);
    break;
  default:
    glBegin(GL_POLYGON);
    break;
  }

  // Loop and Draw
  for (uint32_t i=0;i<mMesh.nrOfElements();i++) 
  {
	std::vector<VisusMeshData::IndexDataType>& element = mMesh.element(i);
        
    // Draw Values
    for (it=element.begin();it!=element.end();it++) 
    { 
      VisusMeshData::IndexDataType idx = *it;
      //glTexCoord1f(mMesh.vertex(idx)[mColorIndex]);
      setColor(*colorMap, mMesh.vertex(idx)[mColorIndex]);
      glNormal3fv(mMesh.componentAddress(idx,mNormalIndex));
      glVertex3fv(mMesh.vertexAddress(idx));
    }
  }

  glEnd();

  if (colorMap->unlockAfterRead() == 0) {
    vwarning("Could not unlock color map after renderin. Aborting rendring.");
    return;
  }
  
}

void VisusMeshDisplay::renderColoredFlat()
{
  std::vector<VisusMeshData::IndexDataType>::iterator it;

  glShadeModel(GL_FLAT);
  switch (mMesh.elementDim()) {
  case 3:
    glBegin(GL_TRIANGLES);
    break;
  case 4:
    glBegin(GL_QUADS);
    break;
  default:
    glBegin(GL_POLYGON);
    break;
  }

  for (uint32_t i=0;i<mMesh.nrOfElements();i++) 
  {
	std::vector<VisusMeshData::IndexDataType>& element = mMesh.element(i);
    
    for (it=element.begin();it!=element.end();it++) 
    { 
      VisusMeshData::IndexDataType idx = *it;
      glTexCoord1f(mMesh.vertex(idx)[mColorIndex]);
      glVertex3fv(mMesh.vertexAddress(idx));
    }
  }

  glEnd();
}

void VisusMeshDisplay::renderSmooth()
{
  std::vector<VisusMeshData::IndexDataType>::iterator it;
  
  glShadeModel(GL_SMOOTH);
  switch (mMesh.elementDim()) {
  case 3:
    glBegin(GL_TRIANGLES);
    break;
  case 4:
    glBegin(GL_QUADS);
    break;
  default:
    glBegin(GL_POLYGON);
    break;
  }

  //fprintf(stderr,"VisusMeshDisplay::display %d elements\n",mMesh.nrOfElements());
  
  for (uint32_t i=0;i<mMesh.nrOfElements();i++) 
  {
	std::vector<VisusMeshData::IndexDataType>& element = mMesh.element(i);
    
    for (it=element.begin();it!=element.end();it++) 
    {
      VisusMeshData::IndexDataType idx = *it;
      //fprintf(stderr,"%f %f %f\n",mMesh.componentAddress(idx,mNormalIndex)[0],mMesh.componentAddress(idx,mNormalIndex)[1],mMesh.componentAddress(idx,mNormalIndex)[2]);
      glNormal3fv(mMesh.componentAddress(idx,mNormalIndex));
      glVertex3fv(mMesh.vertexAddress(idx));
    }
  }

  glEnd();
}

void VisusMeshDisplay::renderFlat()
{
  std::vector<VisusMeshData::IndexDataType>::iterator it;
  
  glShadeModel(GL_FLAT);
  switch (mMesh.elementDim()) {
  case 3:
    glBegin(GL_TRIANGLES);
    break;
  case 4:
    glBegin(GL_QUADS);
    break;
  default:
    glBegin(GL_POLYGON);
    break;
  }

  for (uint32_t i=0;i<mMesh.nrOfElements();i++) 
  {
	std::vector<VisusMeshData::IndexDataType>& element = mMesh.element(i);
    
    glNormal3fv(mMesh.elementNormalAddress(i));
    for (it=element.begin();it!=element.end();it++) {
      glVertex3fv(mMesh.vertexAddress(*it));
    }
  }
  
  glEnd();
}

void VisusMeshDisplay::toXMLLocalVariables(XMLNode& parent)
{
  if (strcmp(XML_TAG, parent.getName()))
    vwarning("VisusMeshDisplay did not receive expected top node");

  parent.addAttribute("normalIndex", mNormalIndex);
  parent.addAttribute("colorIndex", mColorIndex);
  parent.addAttribute("polygonMode", mPolygonMode);
  if (mColorIndex >= 0) 
  {
    parent.addAttribute("min", mMinValue);
    parent.addAttribute("max", mMaxValue);
    parent.addAttribute("range", mValueRange);
  }

  parent.addAttribute("meshProducer", mInputNodes[0]->id());
}

bool VisusMeshDisplay::fromXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusMeshDisplay did not receive expected top node");
    return false;
  }

  mNormalIndex = xmltoi(node.getAttribute("normalIndex"), mNormalIndex);
  mColorIndex = xmltoi(node.getAttribute("colorIndex"), mColorIndex);
  mPolygonMode = xmltoi(node.getAttribute("polygonMode"), mPolygonMode);
  if (mColorIndex >= 0) 
  {
    mMinValue=xmltof(node.getAttribute("min"), mMinValue);
    mMaxValue=xmltof(node.getAttribute("max"), mMaxValue);
    mValueRange=xmltof(node.getAttribute("range"), mValueRange);
  }

  // Connect as consumer
  if (! visusXML::connectConsumer(node, "meshProducer", this, 0, &mMesh)) {
    vwarning("failed to connect as consumer while loading xml");
    return false;
  }
  return true;
}
