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
#include "VisusBlockData.h"
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
#include "VisusHistogram.h"
#include "VisusXMLInterface.h"

#include <GL/gl.h>
#include <GL/glut.h>
#include <math.h>
#include <vector>

using namespace std;

//! Root of the scenegraph
pVisusGroup gRoot,gFocus;
pVisusHistogram histogram;

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

const int numPoints = 5;

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

  switch (key) {
  case 'x':
  case 27: 
    exit(0);
  case 'm': 
    {
      // Write XML data
      VisusXMLInterface xml;
      xml.write("restart.xml", gRoot);
      break;
    }
    default:
      break;
  } 
}

VisusBlockData* createData()
{
  std::vector<int> dims;
  dims.push_back(numPoints);
  dims.push_back(2);
  dims.push_back(1);

  VisusBlockData* data = new VisusBlockData();
  data->dataType(PV_FLOAT32);
  data->samples(dims);
  data->reserveSpace();

  printf("\nCreating line 1:\n");

  float* fdata = reinterpret_cast<float*>(data->data());

  int k=0;
  fdata[k++] = 1.0;
  fdata[k++] = 3.0;

  fdata[k++] = 2.0;
  fdata[k++] = 10.0;

  fdata[k++] = 3.0;
  fdata[k++] = 5.5;

  fdata[k++] = 4.0;
  fdata[k++] = 1.2;

  fdata[k++] = 5.0;
  fdata[k++] = 13.0;

  return data;
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
  font.fontSize(3);

  VisusBorderAxis axis;

  // Create Lines
  VisusBlockData* data = createData();

  // Create The Color Bar
  histogram = VisusHistogram::instantiate();
  histogram->titleText("Colored Data");
  histogram->setValue(VisusColor(1,0,0)); 
  histogram->autoAdjustMinMax(true);
  histogram->barFill(false);
  histogram->barWidth(0.75);
  histogram->unitLength(0.8);
  histogram->xUnits(1.5);
  
  axis = histogram->axis(BB_X_AXIS);
  axis.legendText("X");
  std::string labels[] = { "red", "green", "blue", "orange", "yellow", "black", "white" };
  axis.tickLabels(numPoints, labels);
  axis.labelFont(font);
  histogram->axis(BB_X_AXIS, axis);

  axis = histogram->axis(BB_Y_AXIS);
  axis.legendText("Y");
  axis.labelFont(font);
  histogram->axis(BB_Y_AXIS, axis);

  // Connect to Data
  histogram->loadData(data, 0);

  gRoot->attachSubTree(histogram);
  gFocus = histogram;

  // Create Second Line Graph
  histogram = VisusHistogram::instantiate();
  histogram->titleText("My Data");
  histogram->position(-0.65,0);
  histogram->orientation(BB_VERTICAL);
  histogram->barColor(VisusColor(0,1,0));
  histogram->backgroundColor(VisusColor(0,0,0.5));
  
  axis = histogram->axis(BB_X_AXIS);
  axis.tickPosition(AXIS_BOTH);
  axis.labelPosition(AXIS_BOTH);
  axis.labelFont(font);
  axis.legendText("X");
  axis.maxValue(6.0);
  histogram->axis(BB_X_AXIS, axis);

  axis = histogram->axis(BB_Y_AXIS);
  axis.tickPosition(AXIS_BOTH);
  axis.labelPosition(AXIS_BOTH);
  axis.labelFont(font);
  axis.legendText("Y");
  axis.maxValue(15.0);
  histogram->axis(BB_Y_AXIS, axis);

  // Connect To Data
  histogram->loadData(data, 0);

  gRoot->attachSubTree(histogram);

  glutMainLoop();
  return 1;
}

