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


#include <math.h>
#include <vector>

#include "VisusStdInt.h"

#include "VisusHeightField.h"
#include "VisusSharedColorMap.h"
#include "VisusSharedDataRequest.h"

pVisusHeightField VisusHeightField::instantiate()
{
  return gObjectFactory.instantiate<VisusHeightField>();
}

VisusHeightField::VisusHeightField() : VisusGroup(VISUS_HEIGHT_FIELD), VisusConsumer(2), 
  mOrientation(VISUS_ORIENTATION_XY)
{
  declareParameter<VisusSharedColorMap>();
  declareParameter<VisusSharedDataRequest>();

  mSinks[HF_HEIGHT] = &mHeightData;
  mSinks[HF_COLOR] = &mColorData;
}

void VisusHeightField::orientation(VisusSliceOrientation o)
{
  VisusTransformation3D matrix;
  VisusDataRequest request;

  mOrientation = o;
  switch (mOrientation) {
    
  case VISUS_ORIENTATION_XY:
    matrix = xyPlane();
    
    break;
  case VISUS_ORIENTATION_XZ:
    matrix = xzPlane();
    break;
  case VISUS_ORIENTATION_YZ:
    matrix = yzPlane();
    break;
  }

  // Load the current request
  getValue(request);
  
  request.transformation().setRotation(matrix);
 

  // Share the new request
  setValue(request);

  // If we have a valid producer
  if (this->mInputNodes[0] != NULL)
    this->mInputNodes[0]->interrupt(); // Indicate that a new request has been posted   

  // Indicate that we need to redraw the scenegraph
  markAsDirty();
}


void VisusHeightField::translateRequest(float x, float y)
{
  VisusDataRequest request;
  VisusBoundingBox bbox;
  VisusOpenGLState state;
  VisusTransformation3D acc;
  std::vector<float> trans;

  //fprintf(stderr,"VisusHeightField::translateRequest  %f %f \n",x,y);
  
  accumulate3D(acc);

  getValue(request);
  getValue(bbox);
  getValue(state);

  trans = request.transformation().translation(acc,state,x,-y);

  trans[0] *= (bbox[3] - bbox[0]);
  trans[1] *= (bbox[4] - bbox[1]);
  trans[2] *= (bbox[5] - bbox[2]);
  
  request.transformation()[12] += trans[0];
  request.transformation()[13] += trans[1];
  request.transformation()[14] += trans[2];

  setValue(request);

  // Indicate that we need to redraw the scenegraph
  markAsDirty();
}
    
void VisusHeightField::shiftRequest(float x, float y)
{
  VisusTransformation3D local;
  VisusDataRequest request;
  VisusOpenGLState state;
  float shift[3];

  // First accumulate the transformations from the root to this node
  VisusTransformation3D acc;
  accumulate3D(acc);

  y *= -1;

  getValue(request);
  getValue(state);

  local = request.transformation();

  // Store the translation before we do anything
  shift[0] = local[12];
  shift[1] = local[13];
  shift[2] = local[14];

  // translate freely
  local.translateFreely(acc,state,x,y);

  //fprintf(stderr,"VisusHeightField::shiftRequest   <%f,%f,%f>\n",local[12],local[13],local[14]);

  // Now reset one coordinate to ensure we do not move outside of our plane
  switch (mOrientation) {
  case VISUS_ORIENTATION_XY:
    local[14] = shift[2];
    break;
  case VISUS_ORIENTATION_XZ:
    local[13] = shift[1];
    break;
  case VISUS_ORIENTATION_YZ:
    local[12] = shift[0];
    break;
  }

  request.transformation() = local;
  setValue(request);
 
  // If we have a valid producer
  if (this->mInputNodes[0] != NULL)
    this->mInputNodes[0]->interrupt(); // Indicate that a new request has been posted   

  // Indicate that we need to redraw the scenegraph
  markAsDirty();
}
    
int VisusHeightField::connectHeight(pVisusProducer producer)
{
  if (connect(HF_HEIGHT,producer,&mHeightData) == 0) {
    vwarning("Could not connect height data producer.");
    return 0;
  }

  return 1;
}

int VisusHeightField::connectColor(pVisusProducer producer)
{
  if (connect(HF_COLOR,producer,&mColorData) == 0) {
    vwarning("Could not connect color data producer.");
    return 0;
  }

  return 1;
}

void VisusHeightField::displayBoundingBox() const
{
  VisusDataRequest request;
  
  getValue(request);
  
  request.drawBoundingBox();
}

template<typename T>
void VisusHeightField::drawValues(T* data)
{
  float tex_max[2];
  std::vector<double> extent(3,0);  

  mColorData.texMax(tex_max,tex_max+1);
  extent = mColorData.extent();
    
  glColor3f(1,1,1);
  glDisable(GL_LIGHTING);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
    
  // Get Height Data X-Y Dimensions
  std::vector<int> dims = mHeightData.samples();
  const int MapWidth = dims[0];
  const int MapHeight= dims[1];

  // Get "Height" Value Dimension
  double minHeight,maxHeight;
  mHeightData.getFieldRange(minHeight,maxHeight);
  float heightDim = maxHeight - minHeight;

  // Get Box Z-Dimension
  VisusBoundingBox box;
  getValue(box);
  std::vector<double> ru = box.rightUpper();
  std::vector<double> ll = box.leftLower();
  float zdim = ru[2] - ll[2];
  // float zdim = extent[2];

  // Get The Height-Factor
  float heightFactor = 1.0;
  if (zdim != 0)
    heightFactor = zdim / heightDim;

  const int stepSize = 1;
  //const int stepSize = 4;

  const float xfactor = extent[0] / MapWidth;
  const float yfactor = extent[1] / MapHeight;
  
  const float texXFactor = tex_max[0] / MapWidth;
  const float texYFactor = tex_max[1] / MapHeight;

  glBegin(GL_QUADS);

  for (int i = 0; i < MapWidth-1; i+=stepSize)
  {
     for (int j = 0; j < MapHeight-1; j+=stepSize)
     {
       const float minx = i * xfactor;
       const float maxx = (i + stepSize) * xfactor;
       const float miny = j * yfactor;
       const float maxy = (j + stepSize) * yfactor;

       const float tminx = i * texXFactor;
       const float tmaxx = (i + stepSize) * texXFactor;
       const float tminy = j * texYFactor;
       const float tmaxy = (j + stepSize) * texYFactor;

       float Height = data[i + j*MapWidth] * heightFactor;
       glTexCoord2f(tminx, tminy);
       glVertex3f(minx, miny, Height);
    
       Height = data[(i+stepSize) + j*MapWidth] * heightFactor;
       glTexCoord2f(tmaxx, tminy);
       glVertex3f(maxx, miny, Height);

       Height = data[(i+stepSize) + (j+stepSize)*MapWidth] * heightFactor;
       glTexCoord2f(tmaxx, tmaxy);
       glVertex3f(maxx, maxy, Height);

       Height = data[i + (j+stepSize)*MapWidth] * heightFactor;
       glTexCoord2f(tminx, tmaxy);
       glVertex3f(minx, maxy, Height);
     }
  }
  glEnd();
}


void VisusHeightField::display3D(VisusTransformation3D model_view_3D)
{
  VisusTransformation3D local;
  VisusDataRequest request;
  pVisusSharedColorMap color_map;
  bool pushed = false;

  // Update the modelview matrix, request, and colormap
  getValue(local);  
  getValue(request);
  color_map = sharedValue<VisusSharedColorMap>();

  if (!color_map->initialized()) 
    this->mColorData.resetId();

  // Synchronize both inputs
  if (synchronize() > 1)
    this->markAsDirty();
  
  if (color_map->lockToRead() == 0) {
    vwarning("Could not lock color map for reading. Rendering aborted.");
    return;
  }
    
  // If this colormap has not been initialized
  if (!color_map->initialized()) {
    // We want to use the function range of our data to intialize it
    double low,high;

    mColorData.getFieldRange(low,high);
    color_map->setBounds(low,high);
  }

  // Note that we cannot (necessarily) skip the preDraw even if we
  // are not visible. The node also acts as a producer and the
  // preDraw routine "produces" the tetxure. If you really want to
  // skip things completely hide the extractor in which case no new
  // data is coming in which makes preDraw into a no-op
  if ((mColorData.preDraw(*color_map) != 0) && this->mVisible) {
    pushed = true;

    glMatrixMode(GL_MODELVIEW);

    glPushMatrix();
    model_view_3D *= request.transformation();
    model_view_3D *= request.translateToCenter();
    glLoadMatrixf(model_view_3D);

    VisusTransformation3D mat =  request.transformation();
        
    glPushMatrix();
    glMultMatrixf(mColorData.matrix());
    glMultMatrixf(mColorData.cellCentered());

    switch(mHeightData.dataType())
    {
    case PV_RAW:
      vwarning("Cannot draw \"raw\" values.");
      break;
    case PV_CHAR:
      drawValues(reinterpret_cast<char*>(mHeightData.data()));
      break;
    case PV_UCHAR:
      drawValues(reinterpret_cast<unsigned char*>(mHeightData.data()));
      break;
    case PV_INT:
      drawValues(reinterpret_cast<int*>(mHeightData.data()));
      break;
    case PV_INT16:
      drawValues(reinterpret_cast<int16_t*>(mHeightData.data()));
      break;
    case PV_UINT16:
      drawValues(reinterpret_cast<uint16_t*>(mHeightData.data()));
      break;
    case PV_INT32:
      drawValues(reinterpret_cast<int16_t*>(mHeightData.data()));
      break;
    case PV_UINT32:
      drawValues(reinterpret_cast<uint16_t*>(mHeightData.data()));
      break;
    case PV_INT64:
      drawValues(reinterpret_cast<int16_t*>(mHeightData.data()));
      break;
    case PV_UINT64:
      drawValues(reinterpret_cast<uint16_t*>(mHeightData.data()));
      break;
      
    case PV_FLOAT:
    case PV_FLOAT32:
      drawValues(reinterpret_cast<float*>(mHeightData.data()));
      break;
    case PV_FLOAT64:
      drawValues(reinterpret_cast<double*>(mHeightData.data()));
      break;
    case PV_RGB:
      vwarning("Don't know how to draw a height field of RGB's.");
      break;
    case PV_RGBA:
      vwarning("Don't know how to draw a height field of RGBA's.");
      break;
    }
    
    glEnable(GL_LIGHTING);

    glPopMatrix();
  }
  
  if (color_map->unlockAfterRead() == 0) {
    vwarning("Could not unlock color map after renderin. Aborting rendring.");
    return;
  }
  
  mColorData.postDraw();
  

  recurse(model_view_3D);
  

  if (pushed)
    glPopMatrix();
}


void VisusHeightField::toXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusHeightField did not receive top node");
  }

  node.addAttribute("orientation", mOrientation);
  node.addAttribute("heightProducer", mInputNodes[HF_HEIGHT]->id());
  node.addAttribute("colorProducer", mInputNodes[HF_COLOR]->id());
}

bool VisusHeightField::fromXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusHeightField did not receive top node");
    return false;
  }

  mOrientation = (VisusSliceOrientation)xmltoi(node.getAttribute("orientation"), mOrientation);

  // Get/Connect Height Producer
  if (! visusXML::connectConsumer(node, "heightProducer", this, HF_HEIGHT, &mHeightData)) {
    vwarning("VisusHeightField failed to initialize height producer");
    return false;
  }

  // Get/Connect Color Producer
  if (! visusXML::connectConsumer(node, "colorProducer", this, HF_COLOR, &mColorData)) {
    vwarning("VisusHeightField failed to initialize color producer");
    return false;
  }

  return true;
}
