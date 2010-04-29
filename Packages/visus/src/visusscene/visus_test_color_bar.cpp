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

#include <cstdio> 

#include "VisusGroup.h"
#include "VisusSceneNode.h"
#include "VisusDataRequest.h"
#include "VisusDataDescription.h"
#include "VisusBoundingBox.h"
#include "VisusFieldIndex.h"
#include "VisusAssert.h"
#include "VisusTransformation3D.h"
#include "VisusColorMap.h"
#include "VisusDefaultColorMaps.h"
#include "VisusTickMarks.h"
#include "VisusFont.h"
#include "VisusColorBar.h"
#include "VisusXMLInterface.h"

#include <GL/gl.h>
#include <GL/glut.h>
#include <math.h>
#include <vector>

using namespace std;

//! Root of the scenegraph
pVisusGroup gRoot,gFocus;
pVisusColorBar colorBar;

VisusDataDescription gDataset; // Which data set to use
VisusFieldIndex gFieldIndex = 0; // Which field index
vector<int> gSamples; // How many samples does the data have
VisusBoundingBox gBBox; // Bounding box of the data

int gMouseX,gMouseY;
int gPressed;
bool gMouseMotion = false;
int win_height = 600;
int win_width = 800;
static int mod = 0;

VisusBoundingBox constructWorldBox(const VisusBoundingBox& data_box)
{
  float scale_factor;
  float tmp;
  VisusBoundingBox box;

  scale_factor = 10 / (data_box[3] - data_box[0]);

  tmp = 10 / (data_box[4] - data_box[1]);
  if (tmp < scale_factor)
    scale_factor = tmp;

  tmp = 10 / (data_box[5] - data_box[2]);
  if (tmp < scale_factor)
    scale_factor = tmp;

  for (int i=0;i<3;i++) {
    box[i]   = -(data_box[i+3] - data_box[i]) * scale_factor / 2;
    box[i+3] = +(data_box[i+3] - data_box[i]) * scale_factor / 2;
  }

  return box;
}

void changeFocus(pVisusGroup node)
{
  if (gFocus != NULL) 
    gFocus->drawBoundingBox(false);

  gFocus = node;
  node->drawBoundingBox(true);
}

void changeResolution(pVisusGroup node, bool start_end, bool up)
{
  if (!node->hasSharedValue(VisusSharedDataRequest::sTypeIndex)) 
    return;

  VisusDataRequest request;

  node->getValue(request);

  vector<int> start(3),end(3);

  start = request.startStrides();
  end = request.endStrides();

  if (start_end && up) { // increase the starting resolution
    for (int i=0;i<3;i++) {
      start[i] = max(1,start[i] >> 1);
      end[i] = min(end[i],start[i]);
    }
  }
  else if (!start_end && up) { // increase the end resolution
    for (int i=0;i<3;i++) 
      end[i] = max(1,end[i] >> 1);
  }
  else if (start_end && !up) { // decrease the end resolution
    for (int i=0;i<3;i++) 
      start[i] = start[i] << 1;
  }
  else if (!start_end && !up) { // decrease the end resolution
    for (int i=0;i<3;i++) {
      end[i] = end[i] << 1;
      start[i] = max(start[i],end[i]);
    }
  }

  request.startStrides(start);
  request.endStrides(end);

  node->setValue(request);
}

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

void reshape(int x,int y)
{
  glViewport(0, 0, x, y);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-10,10,-10*y/(float)x,10*y/(float)x,-100,100);
}

void display()
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  VisusOpenGLState state;
  state.fetchState();
  gRoot->setValue(state);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  glColor3f(1,1,1);
  glLineWidth(1.0);

  gRoot->display();

  glutSwapBuffers();
}

void redisplay(void)
{
  glutPostRedisplay();
}

void motion(int x, int y)
{
  
  if (!gMouseMotion)
    return;
#ifdef DEBUG
  printf("In motion\n");
#endif
  switch (gPressed) {
  case GLUT_LEFT_BUTTON:
    if (gFocus != NULL) 
      gFocus->rotate((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
    break;
  case GLUT_MIDDLE_BUTTON:
    if (gFocus != NULL) 
      gFocus->translate((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
    break;
  case GLUT_RIGHT_BUTTON:
    gRoot->rotate((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
    break;
  }

  gMouseX = x;
  gMouseY = y;
  redisplay();
}

void mouse_callback(int button,int state,int x,int y)
{
  if (state == GLUT_DOWN) {
    //fprintf(stderr,"Pressed button %d\n",button);
    gPressed = button;
    gMouseMotion = true;
    gMouseX = x;
    gMouseY = y;
  }
  else {
    gMouseMotion = false;
    gPressed = -1;
  }
  redisplay();
}

void keyboard(unsigned char key, int x, int y)
{
  static int i=0;

  mod = glutGetModifiers();
  //if (mod == GLUT_ACTIVE_SHIFT)

  //printf("User pressed key (%c)\n", key);

  switch (key) {
    case 27: 
      exit(0);
    case 'x':
      exit(0);
    case 'm': 
    {
      // Write XML data
      VisusXMLInterface xml;
      xml.write("restart.xml", gRoot);
      break;
    }
    case 'n':
      i=(i+1)%11;
      switch (i) {
        case 0: colorBar->setValue(VisusDefaultColorMaps::banded()); 
          printf("changing color bar to... banded\n");
          break;
        case 1: colorBar->setValue(VisusDefaultColorMaps::bgry()); 
          printf("changing color bar to... bgry\n");
          break;
        case 2: colorBar->setValue(VisusDefaultColorMaps::bry()); 
          printf("changing color bar to... bry\n");
          break;
        case 3: colorBar->setValue(VisusDefaultColorMaps::gamma()); 
          printf("changing color bar to... gamma\n");
          break;
        case 4: colorBar->setValue(VisusDefaultColorMaps::hot1()); 
          printf("changing color bar to... hot1\n");
          break;
        case 5: colorBar->setValue(VisusDefaultColorMaps::hot2()); 
          printf("changing color bar to... hot2\n");
          break;
        case 6: colorBar->setValue(VisusDefaultColorMaps::ice()); 
          printf("changing color bar to... ice\n");
          break;
        case 7: colorBar->setValue(VisusDefaultColorMaps::lighthues()); 
          printf("changing color bar to... lighthues\n");
          break;
        case 8: colorBar->setValue(VisusDefaultColorMaps::lut16()); 
          printf("changing color bar to... lut16\n");
          break;
        case 9: colorBar->setValue(VisusDefaultColorMaps::rich()); 
          printf("changing color bar to... rich\n");
          break;
        case 10:colorBar->setValue(VisusDefaultColorMaps::smooth_rich()); 
          printf("changing color bar to... smooth_rich\n");
          break;
      }
      redisplay();
      break;

    default:
      break;
  } 
}

void idle()
{
  //gRoot->rotate(2.4/(float)win_width,0.1/(float)win_height);
  //redisplay(); 
}

int main(int argc,char *argv[])
{
  glutInit(&argc,argv);
  glutInitWindowSize(win_width,win_height);
  glutInitWindowPosition(200,200);

  glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH  | GLUT_MULTISAMPLE);
  glutCreateWindow("ViSUS 2.0 Color Bar test");

  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutKeyboardFunc(keyboard);
  glutMouseFunc(mouse_callback);
  glutMotionFunc(motion);
  glutIdleFunc(idle);

  glInit();

  VisusBoundingBox Rbox;
  Rbox.set(-5,-5,-5,5,5,5);

  // First create the root
  gRoot = VisusSceneNode::instantiate();
  gRoot->setValue(Rbox);
  gFocus = gRoot;

  VisusFont font;
  font.fontSize(5);

  VisusBorderAxis axis;

  // Create The Color Bar
  colorBar = VisusColorBar::instantiate();
  colorBar->setValue(VisusColor(1,0,0)); 
  axis = colorBar->axis();
  axis.legendText("Pressure");
  // Change the format of the ticks
  //axis.labelText("Label: %0.1f");  
  // User-defined labels
  //std::string tickLabels[5] = {std::string(""),  
  //			       std::string("tick 1"), 
  //			       std::string("tick 2"), 
  //			       std::string("tick 3"),
  //			       std::string("")};
  // axis.tickLabels(5, tickLabels);
  axis.labelFont(font);
  axis.labelOrientation(VISUS_TOP_BOTTOM);
  colorBar->axis(axis);
  gRoot->attachSubTree(colorBar);
  gFocus = colorBar;

  // Create Second Color Bar
  /*colorBar = VisusColorBar::instantiate();
  colorBar->position(-1,0);
  colorBar->orientation(BB_VERTICAL);
  axis = colorBar->axis();
  axis.tickPosition(AXIS_BOTH);
  axis.labelPosition(AXIS_BOTH);
  axis.legendText("Temperature");
  font.fontSize(3);
  axis.legendFont(font);
  font.fontSize(2);
  axis.labelFont(font);
  colorBar->axis(axis);
  gRoot->attachSubTree(colorBar);
  */
  /*
  // Create Third Color Bar
  unsigned char color[4] = { 0, 0, 255, 255};
  font.fontColor(color);
  font.fontSize(3);
  colorBar = VisusColorBar::instantiate();
  colorBar->position(-1,-0.8);
  axis = colorBar->axis();
  axis.tickPosition(AXIS_HIGH);
  axis.legendAlignment(AXIS_LEFT_ALIGN);
  axis.legendPosition(AXIS_HIGH);
  axis.legendText("Density");
  axis.labelPosition(AXIS_HIGH);
  axis.labelFont(font);
  colorBar->axis(axis);
  gRoot->attachSubTree(colorBar);
  */
  // Testing the setting of instantiate to a different pointer type
  //pVisusTickMarks tickMarks = VisusColorBar::instantiate();
  //pVisusGroup group = VisusColorBar::instantiate();

  glutMainLoop();
  return 1;
}

