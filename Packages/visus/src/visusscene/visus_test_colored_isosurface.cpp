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

#define GLEW_STATIC
#include "glew.h"
#include <GL/glut.h>


#include "VisusGroup.h"
#include "VisusDefaultColorMaps.h"
#include "VisusSceneNode.h"
#include "VisusAxisAlignedExtractor.h"
#include "VisusColoredIsoSurface.h"
#include "VisusDataRequest.h"
#include "VisusDataDescription.h"
#include "VisusBoundingBox.h"
#include "VisusFieldIndex.h"
#include "VisusAssert.h"
#include "VisusTransformation3D.h"
#include "VisusLabelNode.h"
#include "VisusColorMap.h"
#include "VisusColorBar.h"
#include "VisusMeshDisplay.h"
#include "VisusDefaultColorMaps.h"
#include "VisusFastLoopDataSource.h"
#include "VisusIncoreDataSource.h"
#include "VisusIncoreEncoder.h"
#include "VisusTextNode.h"
#include "VisusTickMarks.h"
#include "VisusDataSourceFactory.h"
#include "VisusDataProbe.h"
#include "VisusXMLInterface.h"

using namespace std;

//! Root of the scenegraph
pVisusGroup gRoot,gFocus;
pVisusLabelNode gIsoValueLabel;
pVisusMeshDisplay gDisplay;
pVisusColorBar gColorBar;
char gIsoValueText[100];

VisusDataDescription gIsoDataSet; // Which data set to use
VisusDataDescription gColorDataSet;
VisusFieldIndex gFieldIndex = 0; // Which field index
vector<int> gSamples; // How many samples does the data have
VisusBoundingBox gBBox; // Bounding box of the data

float gMinValue=1.0e+10, gMaxValue=-1.0e+10;
int gMouseX,gMouseY;
int gPressed;
bool gMouseMotion = false;
int win_height = 600;
int win_width = 800;
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

  extractor->setValue(gIsoDataSet);
  surface->setValue(request);
  display->normalIndex(3);
 
  // Connect the data flow
  surface->connectIso(extractor);
  display->connectInput(surface);
  

  // Finally change the focus to the slice
  changeFocus(surface);
  //changeFocus(extractor);

  return surface;
}

pVisusColoredIsoSurface addColoredIso(pVisusGroup node)
{
  if (node == NULL) {
    vwarning("Cannot add slice to NULL node.");
    return pVisusColoredIsoSurface();
  }

  VisusDataRequest request;
  vector<double> extent(3);
  vector<double> left(3),right(3);
  vector<int> start(3),end(3);

  // Create the nodes
  pVisusAxisAlignedExtractor extractor1 = VisusAxisAlignedExtractor::instantiate();
  pVisusAxisAlignedExtractor extractor2 = VisusAxisAlignedExtractor::instantiate();  
  pVisusColoredIsoSurface surface = VisusColoredIsoSurface::instantiate();
  gDisplay = VisusMeshDisplay::instantiate();

  // Add the to the scene graph 
  surface->attachSubTree(gDisplay);
  surface->attachSubTree(extractor1);
  surface->attachSubTree(extractor2);

  surface->declareParameter(VISUS_DATAREQUEST_TYPEID);

  node->attachSubTree(surface);  

  // Make sure they share the data request
  extractor1->inherit<VisusSharedDataRequest>(true);
  extractor2->inherit<VisusSharedDataRequest>(true);
  
  // Compute a default request
  extent[0] = 0.5*(gBBox[3] - gBBox[0]);
  extent[1] = 0.5*(gBBox[4] - gBBox[1]);
  extent[2] = 0.5*(gBBox[5] - gBBox[2]);
  
  start[0] = start[1] = start[2] = 1;
  end[0] = end[1] = end[2] = 1;

  request.extent(extent);
  request.setStrides(start,end);
  request.transformation(translationMatrix((gBBox[3]+gBBox[0]) / 2,
                                           (gBBox[4]+gBBox[1]) / 2,
                                           (gBBox[5]+gBBox[2]) / 2));

  // Connect to the data
  extractor1->setValue(gColorDataSet);
  extractor2->setValue(gIsoDataSet);
  surface->setValue(request);
  gDisplay->normalIndex(3);
  gDisplay->colorIndex(6);
  gDisplay->minValue(gMinValue);
  gDisplay->maxValue(gMaxValue);
  
  // Connect the data flow
  surface->connectColor(extractor1);
  surface->connectIso(extractor2);
  gDisplay->connectInput(surface);

  // Finally change the focus to the slice
  changeFocus(surface);

  return surface;
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
    gRoot->rotate((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
    break;
  case GLUT_MIDDLE_BUTTON:
    if (gFocus != NULL) {
      if ((gModifiers & GLUT_ACTIVE_SHIFT) && (gFocus->type() == VISUS_ORTHO_SLICE)) {
        ((pVisusIsoSurface)gFocus)->translateRequest((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
        //gFocus->translate((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
      }
      else {
        gFocus->translate((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
      }
    }
    break;
  case GLUT_RIGHT_BUTTON:
    gRoot->scale((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
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
    gModifiers = glutGetModifiers();
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
    case 'x':
    case 27: // ESC
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
  //gRoot->rotate(0.2/(float)win_width,0.5/(float)win_height);
  if (gRoot->readClearDirty())
    redisplay();
  else 
    sleepmillisec(10);
  
}

VisusDataSource* createIsoData()
{
  std::vector<int> samples(3);
  samples[0] = samples[1] = samples[2] = 50;

  float *blob = new float[samples[0]*samples[1]*samples[2]];
    
  for (int i=0;i<samples[2];i++) {
    for (int j=0;j<samples[1];j++) {
      for (int k=0;k<samples[0];k++) {
        blob[k + j*samples[0] + i*samples[0]*samples[1]] = 
          sqrt(pow(i-25.0,2) + pow(j-25.0,2) + pow(k-25.0,2));
      }
    }
  }

  VisusIncoreEncoder encoder(samples[0],samples[1],samples[2],1);
  encoder.domain(0,0,0,1.0*samples[0],1.0*samples[1],1.0*samples[2]);
  encoder.field(blob);
  std::string dataset_name = encoder.toString();

  gIsoDataSet = dataset_name.c_str();
    
  // Let's see whether we can open the data on its own
  VisusDataSource* data =  VisusDataSourceFactory::make(gIsoDataSet); 
  if (!data->isValid()) {
    fprintf(stderr,"Could not open data set \"%s\".\n", dataset_name.c_str());
    exit(1);
  }
  return data;
}


VisusDataSource* createColorData()
{
  std::vector<int> samples(3);
  std::vector<double> center(3);
  samples[0] = samples[1] = samples[2] = 50;

  const int ndata = samples[0]*samples[1]*samples[2];
  float *blob = new float[ndata];
    
  for (int i=0;i<samples[2];i++) {
    for (int j=0;j<samples[1];j++) {
      for (int k=0;k<samples[0];k++) {
    	float value = i + j*j*100 + k*2;
        blob[k + j*samples[0] + i*samples[0]*samples[1]] = value;
        if (value < gMinValue)
          gMinValue = value;
        if (value > gMaxValue)
          gMaxValue = value;
      }
    }
  }
  VisusIncoreEncoder encoder(samples[0],samples[1],samples[2],1);
  encoder.domain(0,0,0,1.0*samples[0],1.0*samples[1],1.0*samples[2]);
  encoder.field(blob);
  std::string dataset_name = encoder.toString();

  gColorDataSet = dataset_name.c_str();
    
  // Let's see whether we can open the data on its own
  VisusDataSource* data =  VisusDataSourceFactory::make(gColorDataSet); 
  if (!data->isValid()) {
    fprintf(stderr,"Could not open data set \"%s\".\n", dataset_name.c_str());
    exit(1);
  }
  return data;
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

  VisusDataSource* data = createIsoData();
  createColorData();

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

  // Compute an approriate bounding box to contain the data in world space
  VisusBoundingBox box;
  box = constructWorldBox(gBBox);
  
  gRoot->mapToWorldBox(box);
  gRoot->drawBoundingBox(true);

  VisusFont font;
  font.fontSize(3);

  VisusColorMap colorMap = VisusDefaultColorMaps::banded(); 

  gColorBar = VisusColorBar::instantiate();
  gColorBar->position(-1,-0.8);
  gColorBar->setValue(colorMap);
  VisusBorderAxis axis = gColorBar->axis();
  axis.drawLegend(false);
  axis.minValue(gMinValue);
  axis.maxValue(gMaxValue);
  axis.labelFont(font);
  gColorBar->axis(axis);
  gRoot->attachSubTree(gColorBar);

  pVisusColoredIsoSurface surface = addColoredIso(gRoot);

  VisusIsoValue iso = 7;
  surface->setValue(iso);
    
  gIsoValueLabel = VisusLabelNode::instantiate();
  gIsoValueLabel->position(0.8,-0.9);
  gIsoValueLabel->setValue(font);
  updateIso(iso);
  gRoot->attachSubTree(gIsoValueLabel);

  gDisplay->setValue(colorMap);

  glutMainLoop();
  return 1;
}

