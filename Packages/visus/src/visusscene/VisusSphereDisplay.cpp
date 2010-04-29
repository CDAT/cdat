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

#include "VisusSphereDisplay.h"
#include "VisusStdInt.h"


pVisusSphereDisplay VisusSphereDisplay::instantiate()
{
  return gObjectFactory.instantiate<VisusSphereDisplay>();
}


VisusSphereDisplay::VisusSphereDisplay() : VisusGroup(), VisusConsumer(1)
{
  declare<VisusSharedSphereRadius>();
  declareParameter<VisusSharedColorMap>();
}

VisusSphereDsiplay::~VisuSphereDisplay()
{
}

void VisusSphereDisplay::display(VisusTransformation3D model_view_3D,
                                 VisusTransformation2D model_view_2D,
                                 bool lock_graph)
{
  VisusTransformation3D local;
  pVisusSharedColorMap color_map;
  float tex_max[2];

  synchronize();

  if (mDrawBoundingBox)
    displayBoundingBox();

  getValue(local);
  
  // Here we get a pointer to the color map rather than a new color
  // map since we don't want to copy the color map each time we
  // render. Ultimately one should probably share pointers to color
  // maps
  color_map = sharedValue<VisusSharedColorMap>();

  model_view_3D *= local;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadMatrixf(model_view_3D);

  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);


  if (mTexture.preDraw(*color_map) != 0) {
    
    mData.texMax(tex_max,tex_max+1);

    glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

    glBegin(GL_QUADS);
    {
      glTexCoord2f(0,0);                  glVertex3f(0,0,0);
      glTexCoord2f(tex_max[0],0);         glVertex3f(extent[0],0,0);
      glTexCoord2f(tex_max[0],tex_max[1]);glVertex3f(extent[0],extent[1],0);
      glTexCoord2f(0,tex_max[1]);         glVertex3f(0,extent[1],0);
    }
    glEnd();
    


  if (lock_graph && (lockToRead() == 0)) {
    vwarning("Could not lock graph for reading no further recursion in the display function");
    return;
  }
  
  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display(model_view_3D,model_view_2D,false);
  
  if (lock_graph && (unlockAfterRead() == 0)) 
    vwarning("Could not unlock graph after reading");

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


