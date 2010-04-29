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

#include "VisusOrthogonalSlice.h"
#include "VisusSharedColorMap.h"
#include "VisusSharedDataRequest.h"

pVisusOrthogonalSlice VisusOrthogonalSlice::instantiate()
{
  return gObjectFactory.instantiate<VisusOrthogonalSlice>();
}

VisusOrthogonalSlice::VisusOrthogonalSlice() : VisusGroup(VISUS_ORTHO_SLICE), VisusConsumer(1), 
                                               mOrientation(VISUS_ORIENTATION_XY), 
                                               mColorMapVersion(VISUS_NULL_VERSION)
{
  declareParameter<VisusSharedColorMap>();
  declareParameter<VisusSharedDataRequest>();
}

void VisusOrthogonalSlice::orientation(VisusSliceOrientation o)
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
  request = VisusConsumer::latestRequest(0);
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


void VisusOrthogonalSlice::translateRequest(float x, float y)
{
  VisusDataRequest request;
  VisusBoundingBox bbox;
  VisusTransformation3D acc;
  VisusOpenGLState state;
  std::vector<float> trans;

  //fprintf(stderr,"VisusOrthogonalSlice::translateRequest  %f %f \n",x,y);
  
  accumulate3D(acc);

  request = VisusConsumer::latestRequest(0);
  getValue(request);

  getValue(bbox);
  getValue(state);

  fprintf(stderr,"Bounding box: [%f,%f,%f] x [%f,%f,%f]\n",bbox[0],bbox[1],bbox[2],bbox[3],bbox[4],bbox[5]);

  trans = request.transformation().translation(acc,state,x,-y);

  trans[0] *= (bbox[3] - bbox[0]);
  trans[1] *= (bbox[4] - bbox[1]);
  trans[2] *= (bbox[5] - bbox[2]);
  
  request.transformation()[12] += trans[0];
  request.transformation()[13] += trans[1];
  request.transformation()[14] += trans[2];

  setValue(request);

  // If we have a valid producer
  if (this->mInputNodes[0] != NULL)
    this->mInputNodes[0]->interrupt(); // Indicate that a new request has been posted   

  // Indicate that we need to redraw the scenegraph
  markAsDirty();
}
    
void VisusOrthogonalSlice::shiftRequest(float x, float y)
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

  //fprintf(stderr,"VisusOrthogonalSlice::shiftRequest   <%f,%f,%f>\n",local[12],local[13],local[14]);

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
    
int VisusOrthogonalSlice::connectInput(pVisusProducer producer)
{
  if (connect(0,producer,&mData) == 0) {
    vwarning("Could not connect producer.");
    return 0;
  }

  return 1;
}

/*
void VisusOrthogonalSlice::displayBoundingBox() const
{
  VisusDataRequest request;
  
  request = VisusConsumer::latestRequest(0);
  getValue(request);
  
  request.drawBoundingBox();
}
*/

void VisusOrthogonalSlice::freeze()
{
  VisusDataRequest request;
  request = VisusConsumer::latestRequest(0);

  mFrozenRequestTransformation = request.transformation();
  mFrozenRequestTransformation *= request.translateToCenter();
  mFrozenRequestTransformation *= mData.matrix();
  mFrozenRequestTransformation *= mData.cellCentered();

  VisusGroup::freeze();
}

void VisusOrthogonalSlice::display3D(VisusTransformation3D model_view_3D)
{
  VisusTransformation3D local;
  VisusDataRequest request;
  pVisusSharedColorMap color_map;
  VisusBoundingBox bbox;
  bool pushed = false;

  float tex_max[2];
  std::vector<double> extent(3,0);  

  // Update the modelview matrix, request, and colormap
  if (!frozen()) {
    getValue(local);  
    model_view_3D *= local;
  }
  else
    model_view_3D = mFrozen3D;


  request = VisusConsumer::latestRequest(0);
  bbox = request.queryRegion();
  
  //getValue(request);

  color_map = sharedValue<VisusSharedColorMap>();

  // If our colormap has changed and we do not use implicit shaders we
  // must reload the data. 
  if (color_map->version() != mColorMapVersion) {
    mColorMapVersion = color_map->version();
    this->mData.resetId();
  }
  

  if (synchronize() > 1) {
    this->markAsDirty();
    setValue(bbox);
  }

  
  if (color_map->lockToRead() == 0) {
    vwarning("Could not lock color map for reading. Rendering aborted.");
    return;
  }
    
  // If this colormap has not been initialized
  if (!color_map->initialized()) {
    // We want to use the function range of our data to intialize it
    double low,high;

    mData.getFieldRange(low,high);
    color_map->setBounds(low,high);
  }

  glPushMatrix();


  glLoadMatrixf(model_view_3D);
  

  // Note that we cannot (necessarily) skip the preDraw even if we
  // are not visible. The node also acts as a producer and the
  // preDraw routine "produces" the tetxure. If you really want to
  // skip things completely hide the extractor in which case no new
  // data is coming in which makes preDraw into a no-op
  if ((mData.preDraw(*color_map) != 0) && this->mVisible) {
    pushed = true;

    glMatrixMode(GL_MODELVIEW);

    glPushMatrix();

    if (frozen()) {
      glMultMatrixf(mFrozenRequestTransformation);
    }
    else {
      glMultMatrixf(request.transformation());    
      glMultMatrixf(request.translateToCenter());
      glMultMatrixf(mData.matrix());
      glMultMatrixf(mData.cellCentered());
    }

    mData.texMax(tex_max,tex_max+1);
    extent = mData.extent();
    
    glColor3f(1,1,1);
    glDisable(GL_LIGHTING);
    glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
    
    glBegin(GL_QUADS);
    {
      glTexCoord2f(0,0);                  glVertex3f(0,0,0);
      glTexCoord2f(tex_max[0],0);         glVertex3f(extent[0],0,0);
      glTexCoord2f(tex_max[0],tex_max[1]);glVertex3f(extent[0],extent[1],0);
      glTexCoord2f(0,tex_max[1]);         glVertex3f(0,extent[1],0);
    }
    glEnd();
    
    /*
    glDisable(GL_TEXTURE_2D);
    glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
    glBegin(GL_QUADS);
    for (int i=0;i<72;i++) {
      glVertex3f(i*extent[0]/72,0,0.1);
      glVertex3f((i+1)*extent[0]/72,0,0.1);
      glVertex3f((i+1)*extent[0]/72,extent[1]/72,0.1);
      glVertex3f(i*extent[0]/72,extent[1]/72,0.1);
    }
    glEnd();
    */

    glEnable(GL_LIGHTING);

    glPopMatrix();
  }
  
  if (color_map->unlockAfterRead() == 0) {
    vwarning("Could not unlock color map after renderin. Aborting rendring.");
    return;
  }
  
  mData.postDraw();
  
  if (this->mDrawBoundingBox)
    displayBoundingBox();

  // When creating a hierarchy we want to be able to attach things to
  // the slice and have then inherit the transformation. However, a
  // big portion of where the slice draws is given in the request
  // transformation which contains scaling etc. which we do not want
  // the children to inherit. Furthermore, things like continents must
  // still be drawn in the correct coordinate system independent of
  // the x-y position of the slice. The correct thing to do seems to
  // be to adjust the child matrix such that the position of the slice
  // in its normal direction becomes the origin of that coordinate
  switch (mOrientation) {

  case VISUS_ORIENTATION_XY:
    model_view_3D *= translationMatrix(0,0,bbox[2]);
    break;
  case VISUS_ORIENTATION_XZ:
    model_view_3D *= translationMatrix(0,bbox[1],0);
    break;
  case VISUS_ORIENTATION_YZ:
    model_view_3D *= translationMatrix(bbox[0],0,0);
    break;
  }
    
  recurse(model_view_3D);

  //if (pushed)
  glPopMatrix();
}

void VisusOrthogonalSlice::toXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName()))
    vwarning("VisusOrthogonalSlice did not receive top node");

  node.addAttribute("orientation", mOrientation);
  node.addAttribute("colorMapVersion", mColorMapVersion);

  node.addAttribute("sliceProducer", mInputNodes[0]->id());
}


bool VisusOrthogonalSlice::fromXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusOrthogonalSlice did not receive top node");
    return false;
  }

  mOrientation = (VisusSliceOrientation) xmltoi(node.getAttribute("orientation"), mOrientation);
  mColorMapVersion = (VisusVersionNumber) xmltoi(node.getAttribute("colorMapVersion"), mColorMapVersion);

  // Get Slice Producer
  if (! visusXML::connectConsumer(node, "sliceProducer", this, 0, &mData)) {
    vwarning("failed to connect as consumer while loading xml");
    return false;
  }

  return true;
}

