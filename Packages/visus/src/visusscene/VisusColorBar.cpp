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


#include "VisusColorBar.h"
#include "VisusSharedColorMap.h"

#include "glew.h"
#include <math.h>
#include <iostream>

pVisusColorBar VisusColorBar::instantiate()
{
  return gObjectFactory.instantiate<VisusColorBar>();
}


VisusColorBar::VisusColorBar() : VisusBorderBox2D(VISUS_COLOR_BAR)
{
  declareParameter<VisusSharedColorMap>();

  length(0.9);
  width(0.05);

  mXAxis.enable();
  mXAxis.minorTickLength(0.5);
  mXAxis.minorTickThickness(0.002);
}  

void VisusColorBar::display2D(VisusTransformation2D model_view_2D)
{
  // Color Bar Only Uses One Axis - Set To False Just In Case User Set
  mYAxis.disable();

  // Draw Everything In 2D
  render2D(model_view_2D);

 
  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display(model_view_2D);
}


bool VisusColorBar::renderInsideBox(const float boxWidth, const float boxLength)
{
  
  pVisusSharedColorMap color_map;

  color_map = sharedValue<VisusSharedColorMap>();

  if (color_map->lockToRead() == 0) {
    vwarning("Could not lock color map for reading. Rendering aborted.");
    return false;
  }
  
  glBindTexture(GL_TEXTURE_1D, color_map->texId());
  glTexParameterf(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER,GL_LINEAR);
  glTexParameterf(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER,GL_LINEAR);
  glEnable(GL_TEXTURE_1D);

  if (color_map->unlockAfterRead() == 0) {
    vwarning("Could not unlock color map after renderin. Aborting rendring.");
    return false;
  }

  
  glColor3f(1,1,1);
  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

  glBegin(GL_QUADS);

  glTexCoord1f(0);
  glVertex2f(0,boxWidth);
  glVertex2f(0,0);
  
  glTexCoord1f(1);
  glVertex2f(boxLength,0);
  glVertex2f(boxLength,boxWidth);
  
  glEnd();

  glDisable(GL_TEXTURE_1D);
  
  return true;
}
