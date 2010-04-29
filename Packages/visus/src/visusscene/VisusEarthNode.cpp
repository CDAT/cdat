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


#include "glew.h"
#include <GL/glut.h>
#include "VisusMath.h"

#include "xmlParser.h"

#include "VisusEarthNode.h"
#include "VisusSharedDataDescription.h"
#include "VisusSharedDataRequest.h"
#include "VisusXMLInterface.h"

const int VisusEarthNode::sDefaultSlices = 40;
const int VisusEarthNode::sDefaultStacks = 20;
const float VisusEarthNode::sDefaultGeometryScaling = .1;

pVisusEarthNode VisusEarthNode::instantiate()
{
  return gObjectFactory.instantiate<VisusEarthNode>();
}

VisusEarthNode::VisusEarthNode() : VisusGroup(VISUS_EARTH_NODE), VisusConsumer(2),
                                   mSlices(sDefaultSlices), mStacks(sDefaultStacks),
                                   mPeriodic(false),mGeometryScaling(sDefaultGeometryScaling),
                                   mGeometryData(8),mHeightId(NULLID),mColorMapVersion(VISUS_NULL_VERSION)
{
  declareParameter<VisusSharedEarthRadius>();
  declareParameter<VisusSharedColorMap>();


  //! Hook-up the sinks
  mSinks[0] = &mHeightData;
  mSinks[1] = &mTextureData;

  //! Setting the correct data type or empty data to avoid later warnings
  mHeightData.unit(VISUS_POLAR_DEGREES);

}

int VisusEarthNode::resolution(int slices, int stacks)
{
  if ((slices <= 0) || (stacks <= 0)) {
    vwarning("The number of slices and staks must be at least 1. Resolution change ignored");
    return 0;
  }   

  mSlices = slices;
  mStacks = stacks;

  return 1;
}

void VisusEarthNode::periodic(bool p)
{
  mPeriodic = p;
  
  this->markAsDirty(); 
}

int VisusEarthNode::connectHeight(pVisusProducer producer)
{
  if (connect(0,producer,&mHeightData) == 0) {
    vwarning("Could not connect height producer.");
    return 0;
  }

  return 1;
}

int VisusEarthNode::connectTexture(pVisusProducer producer)
{
  if (connect(1,producer,&mTextureData) == 0) {
    vwarning("Could not connect height producer.");
    return 0;
  }

  return 1;
}


void VisusEarthNode::display3D(VisusTransformation3D model_view_3D)
{

  VisusTransformation3D local;
  VisusEarthRadius radius;
  bool periodic;

  if (!frozen()) {
    // Update the modelview matrix 
    getValue(local);  
    model_view_3D *= local;
  }
  else
    model_view_3D = mFrozen3D;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadMatrixf(model_view_3D);
  
  //model_view_3D.print();

  getValue(radius);
  glPushMatrix();
  glScalef(radius.get(),radius.get(),radius.get());

  if (synchronize() > 1)
    this->markAsDirty();
  
  pVisusSharedColorMap color_map = sharedValue<VisusSharedColorMap>();

  // If our colormap has changed and we do not use implicit shaders we
  // must reload the data. 
  if (color_map->version() != mColorMapVersion) {
    mColorMapVersion = color_map->version();
  }

  if (this->mInputLock.readLock() == 0) {
    vwarning("Could not lock inputs for reading.");
    return;
  }

  if (mTextureData.dataSize() > 0) { // If we have some texture data we want to draw it
  
    if (color_map->lockToRead() == 0) {
      vwarning("Could not lock color map for reading. Rendering aborted.");
      return;
    }
    
    // If this colormap has not been initialized
    if (!color_map->initialized()) {
      // We want to use the function range of our data to intialize it
      double low,high;
      
      mTextureData.getFieldRange(low,high);
      color_map->setBounds(low,high);
    }
    
    // Setup the texture 
    mTextureData.preDraw(*color_map);
    
    if (color_map->unlockAfterRead() == 0) {
      vwarning("Could not unlock color map after renderin. Aborting rendring.");

      this->mInputLock.unlock();
      return;
    }
  }
  
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

  glEnable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_RESCALE_NORMAL);
  
  periodic = mPeriodic;
  // If neither the height nor the texture is connected
  if ((mHeightData.dataSize() == 0) && (mTextureData.dataSize() == 0)) {
    float u[2] = {-M_PI,M_PI};
    float v[2] = {-M_PI_2,M_PI_2};

    // Draw a simple sphere 
    glColor3f(0.9,0.5,0.9);
    renderSphere(u,v);

  }
  else if (mHeightData.dataSize() == 0) { // If we have no height field
    float u[2];
    float v[2];

    glEnable(GL_ALPHA_TEST);
    glColor4f(1,1,1,1);
    
    glMatrixMode(GL_TEXTURE);
    glPushMatrix();
    glLoadIdentity();

    textureTransform(u,v);
    
    renderSphere(u,v);
    
    
    glPopMatrix();    
    glMatrixMode(GL_MODELVIEW);
  }
  else if (mTextureData.dataSize() == 0) { // If we have no texture
        
    glColor3f(0.9,0.7,0.9);
    
    //fprintf(stderr,"VisusEarthNode no texture drawing.\n");
    renderGeometry();
  }
  else { // We have both a height and a color
    float u[2];
    float v[2];

    
    glEnable(GL_ALPHA_TEST);
    glColor4f(1,1,1,1);
    
    glMatrixMode(GL_TEXTURE);
    glPushMatrix();
    glLoadIdentity();

    textureTransform(u,v);

    renderGeometry();
    
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);

  }

  glPopMatrix();

  glDisable(GL_RESCALE_NORMAL);
   
  if (mTextureData.dataSize() > 0) 
    mTextureData.postDraw();
    
  if (mInputLock.unlock() == 0) 
    vwarning("Could not unlock inputs.");

  recurse(model_view_3D);
  
  glPopMatrix();

  vglerror();
}



int VisusEarthNode::loadHeight(VisusData* data)
{
  if (VisusConsumer::loadData(data,0) == 0)
    return 0;

  updateGeometry();
  
  this->markAsDirty();
  return 1;
}

int VisusEarthNode::loadTexture(VisusData* data)
{
  if (VisusConsumer::loadData(data,1) == 0)
    return 0;
  
  this->markAsDirty();
  return 1;
}


int VisusEarthNode::synchronize(int n)
{
  int rc = VisusConsumer::synchronize(n);

  // If we have loaded a new piece of data 
  if (mHeightData.id() != mHeightId) {
    if (this->mInputLock.writeLock() == 0) {
      vwarning("Could not obtain write lock to create mesh.");
      return rc;
    }

    updateGeometry();
    
    mHeightId = mHeightData.id();
    
    if (this->mInputLock.unlock() == 0)
      vwarning("Could not unlock mesh.");
  }

  return rc;
}

void VisusEarthNode::toXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusEarthNode did not receive top node");
  }

  node.addAttribute("slices", mSlices);
  node.addAttribute("stacks", mStacks);


  node.addAttribute("periodic", mPeriodic);

  node.addAttribute("heightId", mHeightId);  
  node.addAttribute("colorMapVersion", mColorMapVersion);
}

bool VisusEarthNode::fromXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusEarthNode did not receive top node");
    return false;
  }

  mSlices = xmltoi(node.getAttribute("slices"), mSlices);
  mStacks = xmltoi(node.getAttribute("stacks"), mStacks);

  
  mPeriodic = xmltobool(node.getAttribute("periodic"), mPeriodic);


  mHeightId = xmltoi(node.getAttribute("heightId"), mHeightId);  
  mColorMapVersion = (VisusVersionNumber) xmltoi(node.getAttribute("colorMapVersion"), mColorMapVersion);

  return true;
}
void VisusEarthNode::unitCoordinates(float longitude, float latitude, float coord[3])
{
  //fprintf(stderr,"Computing long %f lat %f\n",longitude,latitude);

  coord[0] = cos(longitude) * cos(latitude);
  coord[1] = sin(longitude) * cos(latitude);
  coord[2] = sin(latitude);
}

void VisusEarthNode::updateGeometry()
{
  VisusDataRequest request;

  if ((mHeightData.unit() != VISUS_POLAR_DEGREES) 
      && (mHeightData.unit() != VISUS_POLAR_RADIANTS)) {
    vwarning("An earth note can only display data in polar coordinates. Data has unit %d.",mHeightData.unit());

    return;
  }

  // bounding box of the data in domain space
  std::vector<double> left_lower(3),right_upper(3); 
  VisusTransformation3D matrix;

  // Get the domain bounding box of the data without the
  // transformation implied by the currently active data request
  mHeightData.getDomainBoundingBox(left_lower,right_upper);
  
  
  //fprintf(stderr,"Domain Box [%f, %f, %f] x [%f, %f, %f]\n",
  //        left_lower[0],left_lower[1],left_lower[2],
  //        right_upper[0],right_upper[1],right_upper[2]);
  

  // Get the current geometry data request
  request = latestRequest(0);

  matrix = request.transformation();
  matrix *= request.translateToCenter();
  
  matrix.transform(&left_lower[0],&left_lower[0]);
  matrix.transform(&right_upper[0],&right_upper[0]);

  
  // At this point left_lower/right_upper store the domain coordiantes
  // of the two bounding box corners. Note, that these two corners do
  // not necessarily describe the new bounding box. However, we ignore
  // this fact for now as it seems to be a non-useful generalization

  // If necessary we transform the coordinates into radiants
  if (mHeightData.unit() == VISUS_POLAR_DEGREES) {
    
    left_lower[0] = left_lower[0]*M_PI / 180;
    left_lower[1] = left_lower[1]*M_PI / 180;
    
    right_upper[0] = right_upper[0]*M_PI / 180;
    right_upper[1] = right_upper[1]*M_PI / 180;
    
  }

  //fprintf(stderr,"Polar bounds <%f %f %f> <%f %f %f>   Unit %d\n",left_lower[0],left_lower[1],left_lower[2],
  //        right_upper[0],right_upper[1],right_upper[2],mHeightData.unit());

  switch (mHeightData.dataType()) {
  case PV_CHAR:
    constructGeometry<int8_t>(left_lower,right_upper);
    break;
  case PV_UCHAR:
    constructGeometry<uint8_t>(left_lower,right_upper);
    break;
  case PV_INT:
    constructGeometry<int>(left_lower,right_upper);
    break;
  case PV_INT16:
    constructGeometry<int16_t>(left_lower,right_upper);
    break;
  case PV_UINT16:
    constructGeometry<uint16_t>(left_lower,right_upper);
    break;
  case PV_INT32:
    constructGeometry<int32_t>(left_lower,right_upper);
    break;
  case PV_UINT32:
    constructGeometry<uint32_t>(left_lower,right_upper);
    break;
  case PV_INT64:
    constructGeometry<int64_t>(left_lower,right_upper);
    break;
  case PV_UINT64:
    constructGeometry<uint64_t>(left_lower,right_upper);
    break;
  case PV_FLOAT:
  case PV_FLOAT32:
    constructGeometry<float>(left_lower,right_upper);
    break;
  case PV_FLOAT64:
    constructGeometry<double>(left_lower,right_upper);
    break;
  default:
    vwarning("Cannot construct geometry from data type %d.",mHeightData.dataType());
    break;
  }

}




     
void VisusEarthNode::renderGeometry()
{
  int p = 0;
  int nx = mGeometryData.element(0)[0]+1;
  //float u,v;

  if (mPeriodic)
    p = 1;

  //glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
  //glDisable(mTextureData.textureTarget());
  for (int j=0;j<mGeometryData.element(0)[0]-1+p;j++) {

    glBegin(GL_QUAD_STRIP);
    for (int i=0;i<mGeometryData.element(0)[1];i++) {
      
      //fprintf(stderr,"<%f,%f>\n",mDataCoordinates[(mSlices+1)*j + i].t[0],
      //        mDataCoordinates[(mSlices+1)*j + i].t[1]);

      
      glTexCoord2fv(mGeometryData.componentAddress(i*nx + j+1,6));
      //glTexCoord2f(u,v);
      glNormal3fv(mGeometryData.componentAddress(i*nx + j+1,3));
      glVertex3fv(mGeometryData.componentAddress(i*nx + j+1,0));

      glTexCoord2fv(mGeometryData.componentAddress(i*nx + j,6));
      //glTexCoord2f(u,v);
      glNormal3fv(mGeometryData.componentAddress(i*nx + j,3));
      glVertex3fv(mGeometryData.componentAddress(i*nx + j,0));

    }
    
    glEnd();
  }

  vglerror();
}

void VisusEarthNode::textureTransform(float u_bounds[2], float v_bounds[2])
{
  VisusDataRequest request;

  if ((mTextureData.unit() != VISUS_POLAR_DEGREES) 
      && (mTextureData.unit() != VISUS_POLAR_RADIANTS)) {
    vwarning("An earth note can only display data in polar coordinates. Data has unit %d.",mTextureData.unit());
    return;
  }

  // bounding box of the data in domain space
  std::vector<double> left_lower(3),right_upper(3); 
  VisusTransformation3D matrix;

  // Get the domain bounding box of the data without the
  // transformation implied by the currently active data request
  mTextureData.getDomainBoundingBox(left_lower,right_upper);
  
  // Get the current geometry data request
  request = latestRequest(1);

  matrix = request.transformation();
  matrix *= request.translateToCenter();
  
  matrix.transform(&left_lower[0],&left_lower[0]);
  matrix.transform(&right_upper[0],&right_upper[0]);

  
   // At this point left_lower/right_upper store the domain coordiantes
  // of the two bounding box corners. Note, that these two corners do
  // not necessarily describe the new bounding box. However, we ignore
  // this fact for now as it seems to be a non-useful generalization

  // If necessary we transform the coordinates into radiants
  if (mTextureData.unit() == VISUS_POLAR_DEGREES) {
    
    left_lower[0] = left_lower[0]*M_PI / 180;
    left_lower[1] = left_lower[1]*M_PI / 180;
    
    right_upper[0] = right_upper[0]*M_PI / 180;
    right_upper[1] = right_upper[1]*M_PI / 180; 
  }

  if ((left_lower[1] < -M_PI_2-0.01) || (right_upper[1] > M_PI_2+0.01))
    vwarning("Invalid latitude coordinates.");

  
  while (left_lower[0] < -M_PI-0.1) {
    left_lower[0] += 2*M_PI;
    right_upper[0] += 2*M_PI;
  }
  while (right_upper[0] > M_PI+0.1) {
    left_lower[0] -= 2*M_PI;
    right_upper[0] -= 2*M_PI;
  }
    
  while (left_lower[1] < -M_PI_2-0.1) {
    left_lower[1] += M_PI;
    right_upper[1] += M_PI;
  }
  while (right_upper[1] > M_PI+0.1) {
    left_lower[1] = M_PI;
    right_upper[1] = M_PI;
  }

  u_bounds[0] = left_lower[0];
  u_bounds[1] = right_upper[0];
  
  v_bounds[0] = left_lower[1];
  v_bounds[1] = right_upper[1];
  
  float tex[2];
  mTextureData.texMax(tex,tex+1);

  //fprintf(stderr,"%f %f  %f %f %d %d %f %f\n",left_lower[0],left_lower[1],right_upper[0],right_upper[1],mTextureData.samples()[0],mTextureData.samples()[1],tex[0],tex[1]);

  // The following transformations take the Radiants space and map it
  // to texture space assuming GL_TEXTURE_RECTANGLE_ARB is used. The
  // reason for the tex[i]-1 and the glTranslatef(0.0,0.5,0) is that
  // this call will adjust for the cell centered drawing of
  // textures. We must take care of this since otherwise we see
  // artifacts for periodic boundaries. 

  glTranslatef(0.5,0.5,0);
  glScalef((tex[0]-1) / (right_upper[0] - left_lower[0]) ,
           (tex[1]-1) / (right_upper[1] - left_lower[1]),1);
  glTranslatef(-left_lower[0],-left_lower[1],0);
}


void VisusEarthNode::renderSphere(float u_bounds[2], float v_bounds[2])
{
  float dx,dy;
  float v[3];
  float longitude;
  float latitude;

  dx = (u_bounds[1] - u_bounds[0]) / (mSlices-1);
  dy = (v_bounds[1] - v_bounds[0]) / (mStacks-1);

  latitude = v_bounds[0];
  for (int j=0;j<mStacks-1;j++) {
    
    longitude = u_bounds[0];

    glBegin(GL_QUAD_STRIP);
    for (int i=0;i<mSlices;i++) {

      unitCoordinates(longitude,latitude+dy,v);

      glTexCoord2f(longitude,latitude+dy);
      //glTexCoord2f(i,j+1);
      //glTexCoord2f(0.5*tex[0]*longitude,tex[1]*(latitude+dy+0.5*M_PI));
      glNormal3fv(v);
      glVertex3fv(v);

      unitCoordinates(longitude,latitude,v);
     
      glTexCoord2f(longitude,latitude);
      //glTexCoord2f(i,j);
      //glTexCoord2f(0.5*tex[0]*longitude,tex[1]*(latitude+0.5*M_PI));
      glNormal3fv(v);
      glVertex3fv(v);
      
      //fprintf(stderr,"Rendering %f %f %f\n",v[0],v[1],v[2]);
      
      longitude += dx;
    }
    glEnd();
    latitude += dy;
  }
}

void VisusEarthNode::computeMeshNormal(int x, int y, int nx, int ny, int periodic_type, float n[3])
{
  float tmp[3];
  float mag;

  vassert(x < nx-1);

  if (x == 0) {

    if (y == 0) {
      computeMeshNormal(y*nx,y*nx + 1, (y+1)*nx,n);
 
      if (periodic_type == 1) {
        computeMeshNormal(y*nx,(y+1)*nx + x, y*nx + nx-2,tmp);
        
        n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
        mag = sqrt(n[0]*n[0] + n[1]*n[1] + n[2]*n[2]);
        n[0] /= mag; n[1] /= mag; n[2] /= mag; 
       
      }
      else if (periodic_type == 0) {
        computeMeshNormal(y*nx,(y+1)*nx + x, y*nx + nx-3,tmp);
        
        n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
        mag = sqrt(n[0]*n[0] + n[1]*n[1] + n[2]*n[2]);
        n[0] /= mag; n[1] /= mag; n[2] /= mag; 
      }
    }
    else if (y == ny-1) {
      computeMeshNormal(y*nx,(y-1)*nx, y*nx + 1,n);
 
      if (periodic_type == 1) {
        computeMeshNormal(y*nx,y*nx + nx-2, (y-1)*nx,tmp);
        
        n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
        mag = sqrt(n[0]*n[0] + n[1]*n[1] + n[2]*n[2]);
        n[0] /= mag; n[1] /= mag; n[2] /= mag; 
      }
      else if (periodic_type == 0) {
        computeMeshNormal(y*nx,y*nx + nx-3, (y-1)*nx,tmp);
        
        n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
        mag = sqrt(n[0]*n[0] + n[1]*n[1] + n[2]*n[2]);
        n[0] /= mag; n[1] /= mag; n[2] /= mag; 
      }
    }
    else {
      
      computeMeshNormal(y*nx,(y-1)*nx, y*nx+1,n);
      
      computeMeshNormal(y*nx,y*nx +1, (y+1)*nx,tmp);
      n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
              
      
      if (periodic_type == 1) {
        computeMeshNormal(y*nx,(y+1)*nx, y*nx+nx-2,tmp);
        n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
        
       computeMeshNormal(y*nx,y*nx + nx-2, (y-1)*nx,tmp);
       n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
      }
      else if (periodic_type == 0) {
        computeMeshNormal(y*nx,(y+1)*nx, y*nx+nx-3,tmp);
        n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
        
       computeMeshNormal(y*nx,y*nx + nx-3, (y-1)*nx,tmp);
       n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
      }

      mag = sqrt(n[0]*n[0] + n[1]*n[1] + n[2]*n[2]);
      n[0] /= mag; n[1] /= mag; n[2] /= mag; 
    }
  }
  else { // x != 0

    if (y == 0) {
      computeMeshNormal(y*nx+x,y*nx+x + 1, (y+1)*nx+x,n);
      
      computeMeshNormal(y*nx+x,(y+1)*nx + x, y*nx+x - 1,tmp);  
      n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
      n[0] /= 2; n[1] /= 2; n[2] /= 2; 
    }
    else if (y == ny-1) {
      computeMeshNormal(y*nx+x,(y-1)*nx+x, y*nx+x + 1,n);
 
      computeMeshNormal(y*nx+x,y*nx+x - 1, (y-1)*nx+x,tmp);       
      n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
      n[0] /= 2; n[1] /= 2; n[2] /= 2; 
    }
    else {
      
      computeMeshNormal(y*nx+x,(y-1)*nx+x, y*nx+x +1,n);
      
      computeMeshNormal(y*nx+x,y*nx+x + 1, (y+1)*nx+x,tmp);
      n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
  
      computeMeshNormal(y*nx+x,(y+1)*nx+x, y*nx-1,tmp);
      n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];
       
      computeMeshNormal(y*nx+x,y*nx+x - 1, (y-1)*nx+x,tmp);
      n[0] += tmp[0]; n[1]+=tmp[1]; n[2] += tmp[2];

      mag = sqrt(n[0]*n[0] + n[1]*n[1] + n[2]*n[2]);
      n[0] /= mag; n[1] /= mag; n[2] /= mag; 
    }
  }
}


void VisusEarthNode::computeMeshNormal(int center, int right, int left, float n[3])
{
  float u[3],v[3];
  float mag;
  float delta;

  for (int i=0;i<3;i++) {
    u[i] = mGeometryData.vertex(right)[i] - mGeometryData.vertex(center)[i];
    v[i] = mGeometryData.vertex(left)[i] - mGeometryData.vertex(center)[i];
  }


  n[0] = u[1]*v[2] - u[2]*v[1];
  n[1] = u[2]*v[0] - u[0]*v[2];
  n[2] = u[0]*v[1] - u[1]*v[0];

  mag = sqrt(n[0]*n[0] + n[1]*n[1] + n[2]*n[2]);
  delta = sqrt(u[0]*u[0] + u[1]*u[1] + u[2]*u[2]
               + v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);

  if (mag > 0.00001*delta) {
    n[0] /= mag;
    n[1] /= mag;
    n[2] /= mag;
  }
  else if (mGeometryData.vertex(center)[2] <= 0) {
    n[0] = 0;
    n[1] = 0;
    n[2] = -1;
  }
  else {
    n[0] = 0;
    n[1] = 0;
    n[2] = 1;
  }
}
