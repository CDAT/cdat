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
#include "VisusHeightField.h"
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
#include "VisusIncoreEncoder.h"
#include "VisusTextNode.h"
#include "VisusTickMarks.h"
#include "VisusDataSourceFactory.h"
#include "VisusDataProbe.h"
#include "VisusXMLInterface.h"

using namespace std;

//! Root of the scenegraph
pVisusGroup gRoot,gFocus;
pVisusHeightField gHeightField;

VisusDataDescription gDataset1; // Which data set to use
VisusDataDescription gDataset2;
VisusFieldIndex gFieldIndex = 0; // Which field index
vector<int> gSamples; // How many samples does the data have
VisusBoundingBox gBBox; // Bounding box of the data

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


pVisusHeightField addSlice(pVisusGroup node, const VisusDataDescription& color, const VisusDataDescription& height)
{
  if (node == NULL) {
    vwarning("Cannot add slice to NULL node.");
    return pVisusHeightField();
  }

  VisusDataRequest request;
  vector<double> extent(3);
  vector<double> left(3),right(3);
  vector<int> start(3),end(3);

  // Create the two nodes
  pVisusAxisAlignedExtractor extractor1 = VisusAxisAlignedExtractor::instantiate();
  pVisusAxisAlignedExtractor extractor2 = VisusAxisAlignedExtractor::instantiate();
  pVisusHeightField hf = VisusHeightField::instantiate();

  // Add the to the scene graph 
  hf->attachSubTree(extractor1);
  hf->attachSubTree(extractor2);
  node->attachSubTree(hf);  

  // Make sure they share the data request
  extractor1->inherit<VisusSharedDataRequest>(true);
  extractor2->inherit<VisusSharedDataRequest>(true);
  
  VisusDataSource *data;
  data =  VisusDataSourceFactory::make(color); 
  if (!data->isValid()) {
    fprintf(stderr,"Could not open data set \n");
    exit(1);
  }
  data->domainBoundingBox(left,right);

  // Compute a default request
  extent[0] = 0.8*(right[0] - left[0]);
  extent[1] = 0.8*(right[1] - left[1]);
  //extent[2] = 1.05*(right[2] - left[2]);
  extent[2] = 0;
  
  start[0] = start[1] = start[2] = 8;
  end[0] = end[1] = end[2] = 4;

  request.requestType(VISUS_2D_REQUEST);
  request.extent(extent);
  request.setStrides(start,end);
  request.transformation(translationMatrix((left[0]+right[0]) / 2.0,
                                           (left[1]+right[1]) / 2.0,
                                           (left[2]+right[2]) / 2.0));

  extractor1->setValue(color);
  extractor2->setValue(height);
  hf->setValue(request);

  // Connect the data flow
  hf->connectColor(extractor1);
  hf->connectHeight(extractor2);
  hf->inherit(VISUS_BOUNDINGBOX_TYPEID,true);

  // Finally change the focus to the slice
  changeFocus(hf);
  //changeFocus(extractor);

  return hf;
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
    gRoot->rotate((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
    break;
  case GLUT_MIDDLE_BUTTON:
    if (gFocus != NULL) {
      if ((gModifiers & GLUT_ACTIVE_SHIFT) && (gFocus->type() == VISUS_ORTHO_SLICE)) {
        ((pVisusHeightField)gFocus)->translateRequest((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
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
  fprintf(stderr,"key pressed \"%c\"\n",key);
  switch (key) {
  case 27: // ESC
      exit(0);
      break;
  case 'x':
      exit(0);
      break;
  case 'm': 
    {
      // Write XML data
      VisusXMLInterface xml;
      xml.write("restart.xml", gRoot);
      break;
    }
  case '1':
    if ((gFocus != NULL) && (gFocus->type() == VISUS_ORTHO_SLICE)) 
      ((pVisusHeightField)gFocus)->orientation(VISUS_ORIENTATION_XY);
    break;
  case '2':
    if ((gFocus != NULL) && (gFocus->type() == VISUS_ORTHO_SLICE)) {
      ((pVisusHeightField)gFocus)->orientation(VISUS_ORIENTATION_XZ);
      fprintf(stderr,"Changing orientation\n");
    }
    break;
  case '3':
    if ((gFocus != NULL) && (gFocus->type() == VISUS_ORTHO_SLICE)) 
      ((pVisusHeightField)gFocus)->orientation(VISUS_ORIENTATION_YZ);
    break;
  case 'a':
    if (gFocus != NULL) {
      VisusTransformation3D trans3D;
      gFocus->getValue(trans3D);
      trans3D.translationStyle(ALIGNED_TRANSLATION);
      gFocus->setValue(trans3D);
      printf("Set translation style to aligned\n");
    }
    break;
  case 'f':
    if (gFocus != NULL) {
      VisusTransformation3D trans3D;
      gFocus->getValue(trans3D);
      trans3D.translationStyle(FREE_TRANSLATION);
      gFocus->setValue(trans3D);
      printf("Set translation style to free\n");
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
  case 9: // tab
    if (gFocus != NULL) {
      gFocus = gFocus->nextNode(VISUS_DISPLAY_NODE);
      changeFocus(gFocus);
    }
    break;
    
  default:
    break;
  } 
}

void idle()
{
  //gRoot->rotate(0.2/(float)win_width,0.5/(float)win_height);
  if (gRoot->readClearDirty())
    redisplay();
  else 
    sleepmillisec(10);
  
}

VisusDataSource* createData1()
{
  std::vector<int> samples(3);
  std::vector<double> center(3);
  samples[0] = samples[1] = samples[2] = 100;
  center[0] = 0.5*(samples[0]-1);
  center[1] = 0.5*(samples[1]-1);
  center[2] = 0.5*(samples[2]-1);

  float *blob = new float[samples[0]*samples[1]*samples[2]];
    
  int c;
  for (int i=0;i<samples[2];i++) {
    for (int j=0;j<samples[1];j++) {
      for (int k=0;k<samples[0];k++) {
        blob[k + j*samples[0] + i*samples[0]*samples[1]] = 
          sqrt(pow(i-center[2],2) + pow(j-center[1],2) + pow(k-center[0],2));
      }
    }
  }

  VisusIncoreEncoder encoder(samples[0],samples[1],samples[2],1);
  encoder.domain(0,0,0,1.0*samples[0],1.0*samples[1],0.25*samples[2]);
  encoder.field(blob);
  std::string dataset_name = encoder.toString();

  gDataset1 = dataset_name.c_str();
    
  // Let's see whether we can open the data on its own
  VisusDataSource* data =  VisusDataSourceFactory::make(gDataset1); 
  if (!data->isValid()) {
    fprintf(stderr,"Could not open data set \"%s\".\n", dataset_name.c_str());
    exit(1);
  }
  return data;
}


VisusDataSource* createData2()
{
  std::vector<int> samples(3);
  std::vector<double> center(3);
  samples[0] = samples[1] = samples[2] = 100;
  center[0] = 0.5*(samples[0]-1);
  center[1] = 0.5*(samples[1]-1);
  center[2] = 0.5*(samples[2]-1);

  float *blob = new float[samples[0]*samples[1]*samples[2]];
    
  int c;
  for (int i=0;i<samples[2];i++) {
    for (int j=0;j<samples[1];j++) {
      for (int k=0;k<samples[0];k++) {
        blob[k + j*samples[0] + i*samples[0]*samples[1]] = i + j*j*100 + k*2;
      }
    }
  }
  VisusIncoreEncoder encoder(samples[0],samples[1],samples[2],1);
  encoder.domain(0,0,0,1.0*samples[0],1.0*samples[1],0.25*samples[2]);
  encoder.field(blob);
  std::string dataset_name = encoder.toString();

  gDataset2 = dataset_name.c_str();
    
  // Let's see whether we can open the data on its own
  VisusDataSource* data =  VisusDataSourceFactory::make(gDataset2); 
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

  VisusDataSource* data = createData1();
  data = createData2();
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

  gHeightField = addSlice(gRoot, gDataset2, gDataset1);
  gHeightField->setValue(VisusDefaultColorMaps::banded());
  gHeightField->orientation(VISUS_ORIENTATION_XY);
  
  glutMainLoop();
  return 1;
}

