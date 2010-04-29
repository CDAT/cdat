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

#include <cstdio> 
#include <math.h>
#include <vector>
#include <iostream>
#include <fstream>

#include "glew.h"
#include <GL/glut.h>


#include "VisusSceneNode.h"
#include "VisusSphereNode.h"
#include "VisusSphereSlice.h"
#include "VisusDataRequest.h"
#include "VisusDataDescription.h"
#include "VisusBoundingBox.h"
#include "VisusFieldIndex.h"
#include "VisusAssert.h"
#include "VisusTransformation3D.h"
#include "VisusColorMap.h"
#include "VisusDefaultColorMaps.h"
#include "VisusFastLoopDataSource.h"
#include "VisusIncoreDataSource.h"
#include "VisusTextNode.h"
#include "VisusDataSourceFactory.h"
#include "VisusDataProbe.h"

using namespace std;

//! Root of the scenegraph
pVisusGroup gRoot,gFocus;

VisusDataDescription gDataset; // Which data set to use
VisusFieldIndex gFieldIndex = 0; // Which field index
vector<int> gSamples; // How many samples does the data have
VisusBoundingBox gBBox; // Bounding box of the data
 
int gMouseX,gMouseY;
int gPressed;
bool gMouseMotion = false;
int win_height = 600;
int win_width = 800;

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
  
  fprintf(stderr,"New resolution <%d,%d,%d> <%d,%d,%d>\n",start[0],
          start[1],start[2],end[0],end[1],end[2]);
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
  switch (key) {
  case '>':
    if (gFocus != NULL) {
      gFocus = gFocus->nextNode(VISUS_ORTHO_SLICE);
      changeFocus(gFocus);
    }
    break;
  case '+':
    if (gFocus != NULL) {
      changeResolution(gFocus,false,true);
    }
    break;
  case '-':
    if (gFocus != NULL) {
      changeResolution(gFocus,false,false);
    }
    break;
  default:
    break;
  } 
}

void idle()
{
  //gRoot->rotate(0.2/(float)win_width,0.5/(float)win_height);
  redisplay();
  
}

bool loadPPM(pVisusSphereNode sphere, const char *file)
{
  int wid, hgt;

  ifstream in(file, ios::in|ios::binary);
  if (!in) {
	  cerr << "Error: cannot open " << file << endl;
	  return false;
  }
  char header[3];
  in.getline(header, 3);

  int comps = (strcmp(header, "P5") == 0) ? 1 : 3;
  in >> wid >> hgt;

  int val;
  in >> val;
  in.ignore(1024, '\n');

  unsigned int size = comps * wid * hgt;
  unsigned char* data = new unsigned char[size];
    
  in.read((char*)data, size);
  in.close();

  GLenum format = (comps == 1) ? GL_LUMINANCE : GL_RGB;

  sphere->loadTexture(data, wid, hgt, format);    
  return true;
}


int main(int argc,char *argv[])
{
  const char* DEFAULT= "C:/cottom1/visus_workspace/data/200401.ppm";
  char filename[1024], title[1024];
  strcpy(filename, DEFAULT);

  for (int i=0; i<argc; ++i) 
  {
    if (strcmp(argv[i],"-textureFile")==0) {
      ++i;
      strcpy(filename, argv[i]);
    }
  }

  sprintf(title, "ViSUS Sphere Slice - %s", filename);

  glutInit(&argc,argv);
  glutInitWindowSize(win_width,win_height);
  glutInitWindowPosition(200,200);

  glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH  | GLUT_MULTISAMPLE);
  glutCreateWindow(title);

  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutKeyboardFunc(keyboard);
  glutMouseFunc(mouse_callback);
  glutMotionFunc(motion);
  glutIdleFunc(idle);
  
  glInit();

  // First create the root
  gRoot = VisusSceneNode::instantiate();
  gBBox.set(-5,-5,-5,5,5,5);  
  gRoot->setValue(gBBox);
  gFocus = gRoot;

  VisusEarthRadius radius;
  radius.set(2.0);

  // Create Sphere
  pVisusSphereNode sphere = VisusSphereNode::instantiate();
  //loadPPM(sphere, filename);
  sphere->setRadius(2.0);
  gRoot->attachSubTree(sphere);

  // Read File Into Texture
  VisusTexture textureData;
  textureData.loadPPM(filename);

  // Now Create Sphere Slice
  pVisusSphereSlice sphereSlice = VisusSphereSlice::instantiate();
  sphereSlice->setValue(radius);
  sphereSlice->stacks(128);
  sphereSlice->slices(256);
  //sphereSlice->orientation(SS_NON_SPHERE);
  sphereSlice->loadData(&textureData, 0);
  sphere->attachSubTree(sphereSlice);

  glutMainLoop();
  
  return 1;
}

