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
#include "VisusAxisAlignedExtractor.h"
#include "VisusIsoSurface.h"
#include "VisusMeshDisplay.h"
#include "VisusDataRequest.h"
#include "VisusDataDescription.h"
#include "VisusBoundingBox.h"
#include "VisusFieldIndex.h"
#include "VisusAssert.h"
#include "VisusTransformation3D.h"
#include "VisusColorMap.h"
#include "VisusDefaultColorMaps.h"
//#include "VisusFastLoopDataSource.h"
#include "VisusIncoreDataSource.h"
#include "VisusIncoreEncoder.h"
#include "VisusTextNode.h"
#include "VisusDataSourceFactory.h"
#include "VisusLabelNode.h"
#include "VisusXMLInterface.h"

using namespace std;

//! Root of the scenegraph
pVisusGroup gRoot,gFocus;

pVisusLabelNode gIsoValueLabel;
char gIsoValueText[100];

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

void updateIso(const VisusIsoValue& iso)
{
  sprintf(gIsoValueText, "IsoValue: %f", iso.data());
  gIsoValueLabel->text(gIsoValueText);	
}

pVisusIsoSurface addIsoSurface(pVisusGroup node)
{
  if (node == NULL) {
    vwarning("Cannot add slice to NULL node.");
    return pVisusIsoSurface();
  }

  pVisusAxisAlignedExtractor extractor;
  pVisusIsoSurface surface;
  pVisusMeshDisplay display;

  VisusDataRequest request;
  vector<double> extent(3);
  vector<int> start(3),end(3);

  // Create the two nodes
  extractor = VisusAxisAlignedExtractor::instantiate();
  surface = VisusIsoSurface::instantiate();
  display = VisusMeshDisplay::instantiate();

  // Add the to the scene graph 
  node->attachSubTree(extractor);
  extractor->attachSubTree(surface);
  surface->attachSubTree(display);

  // Make sure they share the data request
  //surface->inherit<VisusSharedDataRequest>(true);
  
  // Compute a default request
  extent[0] = 0.5*(gBBox[3] - gBBox[0]);
  extent[1] = 0.5*(gBBox[4] - gBBox[1]);
  extent[2] = 0.5*(gBBox[5] - gBBox[2]);
  //extent[2] = 0;
  
  start[0] = start[1] = start[2] = 1;
  end[0] = end[1] = end[2] = 1;

  request.extent(extent);
  request.setStrides(start,end);
  request.transformation(translationMatrix((gBBox[3]+gBBox[0]) / 2,
                                           (gBBox[4]+gBBox[1]) / 2,
                                           (gBBox[5]+gBBox[2]) / 2));

  extractor->setValue(gDataset);
  extractor->setValue(request);
  display->normalIndex(3);
 
  // Connect the data flow
  surface->connectIso(extractor);
  display->connectInput(surface);
  

  // Finally change the focus to the slice
  changeFocus(surface);
  //changeFocus(extractor);

  return surface;
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
  float light1_ambient[4]  = { 0.7, 0.7, 0.7, 1.0 };
  float light1_diffuse[4]  = { 0.2, 0.2, 0.2, 1.0 };
  float light1_specular[4] = { 1.0, 1.0, 1.0, 1.0 };
  float light1_position[4] = { 1.0, 1.0, -1.0, 0.0 };
  glLightfv(GL_LIGHT1, GL_AMBIENT,  light1_ambient);
  glLightfv(GL_LIGHT1, GL_DIFFUSE,  light1_diffuse);
  glLightfv(GL_LIGHT1, GL_SPECULAR, light1_specular);
  glLightfv(GL_LIGHT1, GL_POSITION, light1_position);
  glEnable(GL_LIGHT1);

  glEnable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);
   
  glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
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
  gRoot->setValue(state,false);

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
  case '+':
    if (gFocus != NULL) {
      VisusIsoValue iso_value;
      gFocus->getValue(iso_value);
      iso_value *= 1.1;
      gFocus->setValue(iso_value);
      updateIso(iso_value);
    }
    break;
  case '-':
    if (gFocus != NULL) {
      VisusIsoValue iso_value;
      gFocus->getValue(iso_value);
      iso_value /= 1.1;
      gFocus->setValue(iso_value);
      updateIso(iso_value);
    }
    break;
  default:
    break;
  }
  redisplay();
}

void idle()
{
  if (gRoot->readClearDirty())
    redisplay();
  else 
	sleepmillisec(10);
}

int main(int argc,char *argv[])
{
  glutInit(&argc,argv);
  glutInitWindowSize(win_width,win_height);
  glutInitWindowPosition(200,200);

  glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH  | GLUT_MULTISAMPLE);
  glutCreateWindow("ViSUS Slice Viewer");

  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutKeyboardFunc(keyboard);
  glutMouseFunc(mouse_callback);
  glutMotionFunc(motion);
  glutIdleFunc(idle);
  
  GLenum err = glewInit();
  if (GLEW_OK != err) {
    /* Problem: glewInit failed, something is seriously wrong. */
    fprintf(stderr, "Error: %s\n", glewGetErrorString(err));
    return 0;
  }
  fprintf(stdout, "Status: Using GLEW %s\n", glewGetString(GLEW_VERSION));
  glInit();

  std::vector<int> samples(3);
  samples[0] = samples[1] = samples[2] = 50;

  float blob[50*50*50];

  for (int i=0;i<50;i++) {
    for (int j=0;j<50;j++) {
      for (int k=0;k<50;k++) {
        
        blob[k + j*50 + i*50*50] = sqrt(pow(i-25.0,2) + pow(j-25.0,2) + pow(k-25.0,2));
      }
    }
  }

  VisusIncoreEncoder encoder(samples[0],samples[1],samples[2],1);
  encoder.domain(-samples[0],-samples[1],-samples[2],1.0*samples[0],1.0*samples[1],1.0*samples[2]);
  encoder.field(blob);
  std::string dataset_name = encoder.toString();

  gDataset = dataset_name.c_str();

  // Let's see whether we can open the data on its own
  VisusIncoreDataSource* data =  (VisusIncoreDataSource*)VisusDataSourceFactory::make(gDataset); 
  if (!data->isValid()) {
    fprintf(stderr,"Could not open data set \"%s\".\n", gDataset.c_str());
    exit(1);
  }

  gSamples = data->samples();
  vector<double> left(3),right(3);
  data->domainBoundingBox(left,right);
  gBBox.set(left,right);

  // close the file again
  delete data;


  // First create the root
  gRoot = VisusGroup::instantiate();

  // Set the bounding box
  gRoot->setValue(gBBox);

  // Compute an approriate bounding box to contain the data in world
  // space
  VisusBoundingBox box;
  box = constructWorldBox(gBBox);
  
  gRoot->mapToWorldBox(box);
  gRoot->drawBoundingBox(true);


  pVisusIsoSurface surface;
  surface = addIsoSurface(gRoot);

  VisusIsoValue iso = 7;
  surface->setValue(iso);
  //surface->drawBoundingBox(true);

  VisusFont font;
  font.fontSize(3);
  
  gIsoValueLabel = VisusLabelNode::instantiate();
  gIsoValueLabel->position(0.8,-0.9);
  gIsoValueLabel->setValue(font);
  updateIso(iso);
  gRoot->attachSubTree(gIsoValueLabel);
  
  
  glutMainLoop();
  return 1;
}

