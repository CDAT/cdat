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
#include <GL/glew.h>
#include <windows.h>
#endif

#include <iostream> 

#include "VisusAssert.h"
#include "VisusBoundingBox.h"
#include "VisusColorBar.h"
#include "VisusDefaultColorMaps.h"
#include "VisusFont.h"
#include "VisusRenderer.h"
#include "VisusSceneNode.h"
#include "VisusSphereNode.h"
#include "VisusTickMarks.h"
#include "VisusBorderAxis.h"

#include <GL/gl.h>
#include <GL/glut.h>
#include <math.h>
#include <vector>

using namespace std;

int win_height = 600;
int win_width = 800;

void glInit()
{
  float light1_ambient[4]  = { 1.0, 1.0, 1.0, 1.0 };
  float light1_diffuse[4]  = { 1.0, 0.9, 0.9, 1.0 };
  float light1_specular[4] = { 1.0, 0.7, 0.7, 1.0 };
  float light1_position[4] = { -1.0, 1.0, 1.0, 0.0 };
  glLightfv(GL_LIGHT1, GL_AMBIENT,  light1_ambient);
  glLightfv(GL_LIGHT1, GL_DIFFUSE,  light1_diffuse);
  glLightfv(GL_LIGHT1, GL_SPECULAR, light1_specular);
  glLightfv(GL_LIGHT1, GL_POSITION, light1_position);
  glEnable(GL_LIGHT1);

  glEnable(GL_LIGHTING);


  glClearColor(0,0,0,0);

  glMatrixMode( GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-10,10,-10*win_height/(float)win_width,10*win_height/(float)win_width,-100,100);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

int main(int argc,char *argv[])
{
#ifdef WIN32
  // Note: Windows does not work unless these are called
  glutInit(&argc,argv);
  glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH  | GLUT_MULTISAMPLE);
#endif

  // Create off-screen renderer
  VisusRenderer renderer(win_width,win_height);

  // Ensure It Is Valid
  if (! renderer.isValid()) {
    vwarning("Renderer was not initialized correctly");
    return 1;
  }

  // Set renderer as current drawable to satisfy GLEW - only necessary if calling glewInit()
  renderer.makeCurrent();

  glInit();
 
  // First create the root
  VisusBoundingBox Rbox;
  Rbox.set(-5,-5,-5,5,5,5);
  pVisusGroup gRoot = VisusSceneNode::instantiate();
  gRoot->setValue(Rbox);
  gRoot->drawBoundingBox(true);
 
  VisusOpenGLState state;
  state.fetchState();
  gRoot->setValue(state);

  VisusFont font;
  font.fontSize(3);
  font.fontColor(255,0,0);
 
  VisusBorderAxis axis;
  
  // Create Tick Marks
  pVisusTickMarks zTickMarks = VisusTickMarks::instantiate();
  axis = zTickMarks->axis();
  axis.legendText("Z-Axis");
  axis.tickColor(VisusColor(1,0,0));
  axis.labelFont(font);
  zTickMarks->axis(axis);
  zTickMarks->direction(TM_Z_AXIS);
  gRoot->attachSubTree(zTickMarks);

  pVisusTickMarks xTickMarks = VisusTickMarks::instantiate();
  axis = xTickMarks->axis();
  axis.legendText("X-Axis");
  axis.labelFont(font);
  xTickMarks->axis(axis);
  gRoot->attachSubTree(xTickMarks);

  pVisusTickMarks yTickMarks = VisusTickMarks::instantiate();
  yTickMarks->direction(TM_Y_AXIS);
  axis = yTickMarks->axis();
  axis.legendText("Y-Axis");
  axis.labelFont(font);
  yTickMarks->axis(axis);
  gRoot->attachSubTree(yTickMarks);
  
  // Create Color Bar
  pVisusColorBar colorBar = VisusColorBar::instantiate();
  axis = colorBar->axis();
  axis.legendText("Pressure");
  axis.labelFont(font);
  colorBar->axis(axis);
  colorBar->setValue(VisusDefaultColorMaps::rich());
  gRoot->attachSubTree(colorBar);

  // Render off-screen and write to file
  gRoot->rotate(280/(float)win_width,0.0);
  std::string filename = "renderer-1.ppm";
  if (!renderer.render(gRoot, filename.c_str()))
	std::cerr << "failed to render scene 1" << std::endl;
  else
	std::cout << "Saved scene 1 to file:" << filename.c_str() << std::endl;

  gRoot->rotate(280/(float)win_width,0.0);
  filename = "renderer-2.ppm";
  if (! renderer.render(gRoot, filename.c_str()))
	std::cerr << "failed to render scene 2" << std::endl;
  else
	std::cout << "Saved scene 2 to file:" << filename.c_str() << std::endl;

  return 0;
}

