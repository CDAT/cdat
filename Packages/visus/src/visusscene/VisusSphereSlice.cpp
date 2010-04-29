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

#include "VisusSphereSlice.h"
#include "VisusSharedColorMap.h"
#include "VisusSharedDataRequest.h"

#ifdef DEBUG
FILE* fptr=NULL;
int ntime=0;
#endif

pVisusSphereSlice VisusSphereSlice::instantiate()
{
  return gObjectFactory.instantiate<VisusSphereSlice>();
}

VisusSphereSlice::VisusSphereSlice() : VisusGroup(VISUS_SPHERE_SLICE), VisusConsumer(1), 
  mOrientation(SS_LATITUDE), mXMapping(SS_LONGITUDE), mDirection(SS_POSITIVE),
  mRadiusOffset(1.0), mWidth(1.0), 
  mStart(90), mRotation(180), mPosition(-180),
  mSlices(200), mStacks(10)
{
  declareParameter<VisusSharedColorMap>();
  declareParameter<VisusSharedDataRequest>();
  declareParameter<VisusSharedEarthRadius>();

  mSinks[0] = &mData;
#ifdef DEBUG
  printf("opening quads.txt\n");
  fptr = fopen("c:/quads.txt", "w+");
#endif
}

bool VisusSphereSlice::span(const float startLatOrLong, const float degreeRotation)
{
  switch(mOrientation) 
  {
  case SS_LATITUDE:
    if (startLatOrLong > 90 || startLatOrLong < -90) {
      fprintf(stderr,"VisusSphereSlice: unable to set lattitude span start coordinate out-of-range(%f)\n", 
        startLatOrLong);
      return false;
    }
    break;
  case SS_LONGITUDE:
    if (startLatOrLong > 180 || startLatOrLong < -180) {
      fprintf(stderr,"VisusSphereSlice: unable to set longitude span start coordinate out-of-range(%f)\n", 
        startLatOrLong);
      return false;
    }
    break;
  }

  if (degreeRotation < 1 || degreeRotation > 360) {
      fprintf(stderr,"VisusSphereSlice: unable to set span rotation out-of-range(%f)\n", 
        degreeRotation);
      return false;
  }

  mStart = startLatOrLong;
  mRotation = degreeRotation;
  return true;
}

std::vector<float> VisusSphereSlice::span() const
{
  std::vector<float> values(2,0);
  values[0] = mStart;
  values[1] = mRotation;
  return values;
}

bool VisusSphereSlice::position(const float latOrLong)
{
  switch(mOrientation) 
  {
  case SS_LATITUDE:
    if (latOrLong > 180 || latOrLong < -180) {
      fprintf(stderr,"VisusSphereSlice: unable to set longitude position out-of-range(%f)\n", 
        latOrLong);
      return false;
    }
    break;
  case SS_LONGITUDE:
    if (latOrLong > 90 || latOrLong < -90) {
      fprintf(stderr,"VisusSphereSlice: unable to set lattitude position out-of-range(%f)\n", 
        latOrLong);
      return false;
    }
    break;
  }
  mPosition = latOrLong;
  return true;
}

void VisusSphereSlice::translateRequest(float x, float y)
{
  return;  // no-op for now
}
    
void VisusSphereSlice::shiftRequest(float x, float y)
{
  return; // no-op for now
}
    
int VisusSphereSlice::connectInput(pVisusProducer producer)
{
  if (connect(0,producer,&mData) == 0) {
    vwarning("Could not connect producer.");
    return 0;
  }

  return 1;
}

void VisusSphereSlice::display3D(VisusTransformation3D model_view_3D)
{
  VisusTransformation3D local;
  VisusDataRequest request;

  // Update the modelview matrix and request
  getValue(local);  
  getValue(request);

  model_view_3D *= local;
  //model_view_3D = VisusTransformation3D();
  //model_view_3D *= request.transformation();
  //model_view_3D *= request.translateToCenter();
  //model_view_3D *= mData.matrix();
  
  synchronize();

  if (mDrawBoundingBox)
    displayBoundingBox();

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadMatrixf(model_view_3D);
  //glMultMatrixf(model_view_3D);

  render();

  recurse(model_view_3D);

  glPopMatrix();

#ifdef DEBUG
  if (ntime==0) {
    fclose(fptr);
    printf("closing quads.txt\n");
  }
  ++ntime;
#endif
}

void VisusSphereSlice::render()
{
  VisusEarthRadius earthRadius;
  float tex_max[2];

  std::vector<double> extent(3,0);
  extent[0] = 2.0;
  extent[1] = 2.0;
  mData.extent(extent);

  pVisusSharedColorMap color_map = sharedValue<VisusSharedColorMap>();
  getValue(earthRadius);

  if (color_map->lockToRead() == 0) {
    vwarning("Could not lock color map for reading. Rendering aborted.");
    return;
  }

  if (mData.preDraw(*color_map) != 0) {
    
    mData.texMax(tex_max,tex_max+1);
#ifdef DEBUG
    fprintf(fptr, "Dimensions of texture %f %f\n", tex_max[0], tex_max[1]);
#endif

    glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

    const double sphereRadius = earthRadius.get();

    double delta  = (mRotation / (mSlices-1)) * mDirection;
    double dradius= mWidth*sphereRadius / mStacks;
    double radius = mRadiusOffset + sphereRadius;
#ifdef DEBUG
    if (ntime==0)
      fprintf(fptr, "Initial radius %f ... dradius %f ... delta coord %f\n", radius, dradius, delta);
#endif

    switch (mOrientation)
    {
    case SS_LATITUDE:
    {
      glNormal3f(1,0,0);
      double longitude = mPosition;
      for (int j=0; j<mStacks; ++j, radius+=dradius)
      {
        //glBegin(GL_LINES);
        glBegin(GL_QUADS);
        {
          double lat=mStart;
          if (mStart+mRotation > 90)
            delta *= -1.0;
          for (int i=0; i<mSlices; ++i, lat+=delta) 
          {
#ifdef DEBUG
            if (i%2==0 && ntime==0)
              fprintf(fptr, "\n\nQuad:\n");
#endif
            double nextRadius = radius + dradius;

            double x = cos(longitude) * sin(90 - lat);
            double y = sin(longitude) * sin(90 - lat);
            double z = cos(90 - lat);
             
            double tx =tex_max[0]*i/(mSlices-1.0);
            double ty =tex_max[1]*(j+1)/(mStacks-1.0);

            glTexCoord2f(tx,ty);
            glVertex3f(x*nextRadius, y*nextRadius, z*nextRadius);
#ifdef DEBUG
            if (ntime==0)
              fprintf(fptr, "tex[x,y] %f %f .... maps to [r,lat,long] %f %f %f .. [x,y,z] %f %f %f\n", 
                tx, ty, nextRadius, lat, longitude, x*nextRadius, y*nextRadius, z*nextRadius);
#endif

            ty = tex_max[1]*j / (mStacks-1.0);
            glTexCoord2f(tx, ty);
            glVertex3f(x*radius, y*radius, z*radius);
#ifdef DEBUG
            if (ntime==0)
              fprintf(fptr,"tex[x,y] %f %f .... maps to [r,lat,long] %f %f %f .. [x,y,z] %f %f %f\n", 
                tx, ty, radius, lat, longitude, x*radius, y*radius, z*radius);
#endif
           }
        }
        glEnd();
      }
      break;
    }

    case SS_LONGITUDE:
      break;

    default:
    {
      // Just Show Image
      extent = mData.extent();
      glBegin(GL_QUADS);
      glTexCoord2f(0,0);                  glVertex3f(0,0,0);
      glTexCoord2f(tex_max[0],0);         glVertex3f(extent[0],0,0);
      glTexCoord2f(tex_max[0],tex_max[1]);glVertex3f(extent[0],extent[1],0);
      glTexCoord2f(0,tex_max[1]);         glVertex3f(0,extent[1],0);
      glEnd();
    }

    }

  }
  
  if (color_map->unlockAfterRead() == 0) {
    vwarning("Could not unlock color map after rendering. Aborting rendering.");
    return;
  }
  
  mData.postDraw();
}

