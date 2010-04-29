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

#include "glew.h"
#include <GL/glut.h>

#include "VisusGroup.h"
#include "VisusDataRequest.h"
#include "VisusDataDescription.h"
#include "VisusBoundingBox.h"
#include "VisusFieldIndex.h"
#include "VisusAssert.h"
#include "VisusTransformation3D.h"
#include "VisusColorMap.h"
#include "VisusDefaultColorMaps.h"
#include "VisusLabelNode.h"
#include "VisusFont.h"
#include "VisusSphereNode.h"
#include "VisusSceneNode.h"
#include "VisusColorBar.h"
#include "VisusXMLInterface.h"

using namespace std;

//! Root of the scenegraph
pVisusSceneNode gRoot;
pVisusGroup gFocus;
pVisusLabelNode gLabel;

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
int gModifiers = 0;

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
  gluLookAt(0,0,10,0,0,0,0,1,0);
  glOrtho(-10,10,-10*win_height/(float)win_width,10*win_height/(float)win_width,-100,100);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

void reshape(int x,int y)
{
  glViewport(0, 0, x, y);
  
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluLookAt(0,0,10,0,0,0,0,1,0);
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

  if (gModifiers & GLUT_ACTIVE_SHIFT) {
    switch (gPressed) {
    case GLUT_LEFT_BUTTON: 
      gRoot->scaleJoystick((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
      break;
    case GLUT_MIDDLE_BUTTON:
      gRoot->translateJoystick((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
      break;
    case GLUT_RIGHT_BUTTON:
      gRoot->rotateJoystick((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
      //gRoot->rotateJoystick(0,(y-gMouseY)/(float)win_height);
      //gRoot->rotateJoystick((x-gMouseX)/(float)win_width,0);
      break;
    }
  }
  else {
    switch (gPressed) {
    case GLUT_LEFT_BUTTON:
      gRoot->scale((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
      break;
    case GLUT_MIDDLE_BUTTON:
      gRoot->translate((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
      break;
    case GLUT_RIGHT_BUTTON:
      gRoot->rotate((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
      break;
    }
  } 

  gMouseX = x;
  gMouseY = y;
  redisplay();
}

void mouse_callback(int button,int state,int x,int y)
{
  if (state == GLUT_DOWN) {
    gModifiers = glutGetModifiers();
    //fprintf(stderr,"Pressed button %d\n",button);
    gPressed = button;
    gMouseMotion = true;
    gMouseX = x;
    gMouseY = y;
  }
  else {
    gModifiers = 0;
    gMouseMotion = false;
    gPressed = -1;
  }
  redisplay();
}

void keyboard(unsigned char key, int x, int y)
{
  static float t = 0;
  mod = glutGetModifiers();
  //if (mod == GLUT_ACTIVE_SHIFT)

  switch (key) {
  case 'x':
  case 27:
    exit(0);
    break;
  case 'm': 
    {
      // Write XML data
      VisusXMLInterface xml;
      xml.write("restart.xml", gRoot);
      break;
    } 
  case 's': {
    t = 0;
    FILE* output = fopen("camera.txt","w");
    gRoot->save(output);
    fclose(output);
    break;
  }
  case 'l': {
    FILE* input = fopen("camera.txt","r");
    gRoot->load(input);
    fclose(input);
    break;
  }
  case 'i': {
    VisusCamera cam1,cam2,cam3;
    FILE* input = fopen("origin.txt","r");
    cam1.load(input);
    fclose(input);

    input = fopen("camera.txt","r");
    cam2.load(input);
    fclose(input);
    
    fprintf(stderr,"Interpolationg with factor %f\n",t);
    cam3 = interpolate(cam1,cam2,t);
    t += 0.1;
    gRoot->setValue(cam3);
    break;
  }
    
  default:
      break;
  } 
}

void idle()
{
  //gRoot->rotate(2.4/(float)win_width,0.1/(float)win_height);
  redisplay(); 
}

int main(int argc,char *argv[])
{
  glutInit(&argc,argv);
  glutInitWindowSize(win_width,win_height);
  glutInitWindowPosition(200,200);

  glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH  | GLUT_MULTISAMPLE);
  glutCreateWindow("ViSUS 2.0 Sphere test");

  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutKeyboardFunc(keyboard);
  glutMouseFunc(mouse_callback);
  glutMotionFunc(motion);
  glutIdleFunc(idle);

  glInit();

  // Create the default scene graph 
  vector<double> left(3),right(3);
  left[0]=0;
  left[1]=0;
  left[2]=0;
  right[0]=1;
  right[1]=1;
  right[2]=1;

  gBBox.set(left,right);

  // First create the root
  gRoot = VisusSceneNode::instantiate();
  //gRoot = VisusGroup::instantiate();
  
  //float pos[3] = {0,0,10};
  //gRoot


  gBBox.set(-5,-5,-5,5,5,5);  
  gRoot->setValue(gBBox);

  ////////////////////////////////////////
  // Set the bounding box
  // This will become automatic.
  VisusBoundingBox box;
  box.set(-2,-2,-2,2,2,2);

  /* Create a sphere in the tree. */
  pVisusSphereNode sphere;
  sphere = gObjectFactory.constructNode<VisusSphereNode>();
  changeFocus(sphere);
  sphere->setValue(box);

  gRoot->attachSubTree(sphere);

  /* Add a text node */
  pVisusLabelNode text;
  VisusFont font;
  font.fontSize(8);

  gLabel = VisusLabelNode::instantiate();
  gLabel->text("Visus 2.0 camera test");
  gLabel->setValue(font);
  gLabel->translate(0.01,0);
  gRoot->attachSubTree(gLabel);

  font.fontScaling(VISUS_FONT_ABSOLUTE);
  gLabel = VisusLabelNode::instantiate();
  gLabel->text("Absolute Font");
  gLabel->setValue(font);
  gLabel->position(0.0,-1.0);
  gRoot->attachSubTree(gLabel);


  glutMainLoop();
  return 1;
}

