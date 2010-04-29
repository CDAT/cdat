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

#include "VisusIndexedDisplay.h"
#include "VisusSharedColorMap.h"
#include "VisusSharedColor.h"
#include "VisusBoundingBox.h"
#include "VisusStdInt.h"

pVisusIndexedDisplay VisusIndexedDisplay::instantiate()
{
  return gObjectFactory.instantiate<VisusIndexedDisplay>();
}


VisusIndexedDisplay::VisusIndexedDisplay() : VisusGroup(VISUS_MESH_NODE), VisusConsumer(1), 
                                                     mMesh(new VisusIndexedData(3,VISUS_INDEXED_DATA)),
                                                     mPolygonMode(GL_FILL),mGeometryPrimitive(GL_LINE_STRIP),
                                                     mEnableLighting(false)
{
  declareParameter<VisusSharedColorMap>();
  declareParameter<VisusSharedColor>();
  
  
  // Initially we cannot assume we are given a normal
  mNormalIndex = -1;
  
  // Nor can we assume we have a color index
  mColorIndex = -1;

  mSinks[0] = mMesh;
}


VisusIndexedDisplay::~VisusIndexedDisplay()
{
  delete mMesh;
}

void VisusIndexedDisplay::freeze()
{
  mFrozenDataTransformation = mMesh->matrix();

  VisusGroup::freeze();
}


void VisusIndexedDisplay::display3D(VisusTransformation3D model_view_3D)
{
  VisusTransformation3D local;
  VisusColor color;
  pVisusSharedColorMap color_map;

  if (synchronize() > 1) {
    markAsDirty();
    setValue(mMesh->boundingBox());
  }
    

  getValue(color);

  if (!frozen()) {
    getValue(local);
    model_view_3D *= local;
  }
  else
    model_view_3D = mFrozen3D;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadMatrixf(model_view_3D);

  if (!frozen())
    glMultMatrixf(mMesh->matrix());
  else
    glMultMatrixf(mFrozenDataTransformation);

  if (mDrawBoundingBox) {
    //fprintf(stderr,"draw bounding box\n");
    displayBoundingBox();
  }

  //glPolygonMode(GL_FRONT_AND_BACK,mPolygonMode);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glEnable(GL_COLOR_MATERIAL);

  
  if (!mEnableLighting)
    glDisable(GL_LIGHTING);
    
  if (this->mInputLock.readLock() == 1) {

    if (mMesh->spatialDim() == 2) {
      if ((mColorIndex >= 0) && (mNormalIndex >= 0)) 
        renderColoredSmooth(&VisusIndexedDisplay::vertex2f);
      else if ((mColorIndex >= 0) && (mNormalIndex < 0))
        renderColoredFlat(&VisusIndexedDisplay::vertex2f);
      else if ((mColorIndex < 0) && (mNormalIndex >= 0)) {
        getValue(color);
        color.glColor();
        renderSmooth(&VisusIndexedDisplay::vertex2f);
      }
      else {
        getValue(color);
        color.glColor();
        renderFlat(&VisusIndexedDisplay::vertex2f);
      }
    }
    else if (mMesh->spatialDim() == 3) {
      if ((mColorIndex >= 0) && (mNormalIndex >= 0)) 
        renderColoredSmooth(&VisusIndexedDisplay::vertex3f);
      else if ((mColorIndex >= 0) && (mNormalIndex < 0))
        renderColoredFlat(&VisusIndexedDisplay::vertex3f);
      else if ((mColorIndex < 0) && (mNormalIndex >= 0)) {
        getValue(color);
        color.glColor();
        renderSmooth(&VisusIndexedDisplay::vertex3f);
      }
      else {
        getValue(color);
        color.glColor();
        renderFlat(&VisusIndexedDisplay::vertex3f);
      }
    }

    if (this->mInputLock.unlock() == 0) 
      vwarning("Could not unlock indexed data after rendering."); 
  }
  else {
    vwarning("Could not lock indexed data for drawing.");
  }


  if (!mEnableLighting)
    glEnable(GL_LIGHTING);
  

  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display(model_view_3D);

  glPopMatrix();  
}

int VisusIndexedDisplay::connectInput(pVisusProducer producer)
{
  if (connect(0,producer,mMesh) == 0) {
    vwarning("Could not connect producer.");
    return 0;
  }

  return 1;
}

int VisusIndexedDisplay::loadData(VisusData* data, int input)
{
  if (VisusConsumer::loadData(data,input) != 0) {
    //setValue(mMesh->boundingBox());
    return 1;
  }

  return 0;
}

void VisusIndexedDisplay::renderColoredSmooth(void (VisusIndexedDisplay::*vertexCall)(int))
{
  std::vector<VisusIndexedData::IndexDataType>& element = mMesh->element(0);
  std::vector<VisusIndexedData::IndexDataType>::iterator it;
  
  if ((mGeometryPrimitive == GL_TRIANGLES) ||
      (mGeometryPrimitive == GL_QUADS) ||
      (mGeometryPrimitive == GL_LINES) ||
      (mGeometryPrimitive == GL_POINTS)) {

    glBegin(mGeometryPrimitive);
    for (uint32_t i=0;i<mMesh->nrOfElements();i++) {
      element = mMesh->element(0);
      
      for (it=element.begin();it!=element.end();it++) { 
        glNormal3fv(mMesh->componentAddress(*it,mNormalIndex));
        glTexCoord1f(mMesh->vertex(*it)[mColorIndex]);
        (this->*vertexCall)(*it);
      }
    }
    glEnd();
  }
  else {
    for (uint32_t i=0;i<mMesh->nrOfElements();i++) {
      element = mMesh->element(0);
      
      glBegin(mGeometryPrimitive);
      for (it=element.begin();it!=element.end();it++) { 
        glNormal3fv(mMesh->componentAddress(*it,mNormalIndex));
        glTexCoord1f(mMesh->vertex(*it)[mColorIndex]);
        glVertex3fv(mMesh->vertexAddress(*it));
      }
      glEnd();
    }
  }    
    
}

void VisusIndexedDisplay::renderColoredFlat(void (VisusIndexedDisplay::*vertexCall)(int))
{
  std::vector<VisusIndexedData::IndexDataType>& element = mMesh->element(0);
  std::vector<VisusIndexedData::IndexDataType>::iterator it;
  
  if ((mGeometryPrimitive == GL_TRIANGLES) ||
      (mGeometryPrimitive == GL_QUADS) ||
      (mGeometryPrimitive == GL_LINES) ||
      (mGeometryPrimitive == GL_POINTS)) {

    glBegin(mGeometryPrimitive);
    for (uint32_t i=0;i<mMesh->nrOfElements();i++) {
      element = mMesh->element(i);
      
      for (it=element.begin();it!=element.end();it++) { 
        glTexCoord1f(mMesh->vertex(*it)[mColorIndex]);
        (this->*vertexCall)(*it);
      }
    }
    glEnd();
  }
  else {
    for (uint32_t i=0;i<mMesh->nrOfElements();i++) {
      element = mMesh->element(i);
           
      glBegin(mGeometryPrimitive);
      for (it=element.begin();it!=element.end();it++) { 
        glTexCoord1f(mMesh->vertex(*it)[mColorIndex]);
        (this->*vertexCall)(*it);
      }
      glEnd();
    }
  }
}

void VisusIndexedDisplay::renderSmooth(void (VisusIndexedDisplay::*vertexCall)(int))
{
  std::vector<VisusIndexedData::IndexDataType>& element = mMesh->element(0);
  std::vector<VisusIndexedData::IndexDataType>::iterator it;
  
  if ((mGeometryPrimitive == GL_TRIANGLES) ||
      (mGeometryPrimitive == GL_QUADS) ||
      (mGeometryPrimitive == GL_LINES) ||
      (mGeometryPrimitive == GL_POINTS)) {

    glBegin(mGeometryPrimitive);
    for (uint32_t i=0;i<mMesh->nrOfElements();i++) {
      element = mMesh->element(i);
      
      for (it=element.begin();it!=element.end();it++) { 
        glNormal3fv(mMesh->componentAddress(*it,mNormalIndex));
        (this->*vertexCall)(*it);
      }
    }
    glEnd();
  }
  else {
    for (uint32_t i=0;i<mMesh->nrOfElements();i++) {
      element = mMesh->element(i);
      
      glBegin(mGeometryPrimitive);
      for (it=element.begin();it!=element.end();it++) { 
        glNormal3fv(mMesh->componentAddress(*it,mNormalIndex));
        (this->*vertexCall)(*it);
      }
      glEnd();
    }
  }
}

void VisusIndexedDisplay::renderFlat(void (VisusIndexedDisplay::*vertexCall)(int))
{
  std::vector<VisusIndexedData::IndexDataType>& element = mMesh->element(0);
  std::vector<VisusIndexedData::IndexDataType>::iterator it;
  
  if ((mGeometryPrimitive == GL_TRIANGLES) ||
      (mGeometryPrimitive == GL_QUADS) ||
      (mGeometryPrimitive == GL_LINES) ||
      (mGeometryPrimitive == GL_POINTS)) {

    glBegin(mGeometryPrimitive);
    for (uint32_t i=0;i<mMesh->nrOfElements();i++) {
      element = mMesh->element(i);
      
      for (it=element.begin();it!=element.end();it++) {
        (this->*vertexCall)(*it);
      }
    }
    glEnd();
  }
  else {
    for (uint32_t i=0;i<mMesh->nrOfElements();i++) {
      element = mMesh->element(i);
      
      glBegin(mGeometryPrimitive);
      for (it=element.begin();it!=element.end();it++) {
        (this->*vertexCall)(*it);
        //fprintf(stderr,"Drawing vertex %f %f \n",*mMesh->vertexAddress(*it),*(mMesh->vertexAddress(*it)+1));
      }
      glEnd();
    }
  }
}

