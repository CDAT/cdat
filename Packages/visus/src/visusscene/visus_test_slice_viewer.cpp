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
#include "VisusAxisAlignedExtractor.h"
#include "VisusOrthogonalSlice.h"
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

VisusDataDescription gDataset; // Which data set to use
VisusFieldIndex gFieldIndex = 0; // Which field index
vector<int> gSamples; // How many samples does the data have
VisusBoundingBox gBBox; // Bounding box of the data
 
int gMouseX,gMouseY;
int gPressed;
bool gMouseMotion = false;
int win_height = 600;
int win_width = 800;
int gModifiers = 0;
VisusColorMapType gColorMap = VISUS_GREY_SCALE;

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


pVisusOrthogonalSlice addSlice(pVisusGroup node)
{
  if (node == NULL) {
    vwarning("Cannot add slice to NULL node.");
    return pVisusOrthogonalSlice();
  }

  pVisusAxisAlignedExtractor extractor;
  pVisusOrthogonalSlice slice;
  VisusDataRequest request;
  vector<double> extent(3);
  vector<double> left(3),right(3);
  vector<int> start(3),end(3);

  // Create the two nodes
  extractor = VisusAxisAlignedExtractor::instantiate();
  slice = VisusOrthogonalSlice::instantiate();

  // Add the to the scene graph 
  node->attachSubTree(extractor);
  extractor->attachSubTree(slice);

  // Make sure they share the data request
  slice->inherit<VisusSharedDataRequest>(true);
  
  VisusDataSource *data;
  data =  VisusDataSourceFactory::make(gDataset); 
  if (!data->isValid()) {
    fprintf(stderr,"Could not open data set \n");
    exit(1);
  }
  data->domainBoundingBox(left,right);

  // Compute a default request
  extent[0] = (right[0] - left[0] + 1);
  extent[1] = (right[1] - left[1] + 1);
  //extent[2] = 1.05*(right[2] - left[2]);
  extent[2] = 0;
  
  start[0] = start[1] = start[2] = 8;
  end[0] = end[1] = end[2] = 1;

  request.requestType(VISUS_2D_REQUEST);
  request.extent(extent);
  request.setStrides(start,end);
  request.transformation(translationMatrix((left[0]+right[0]) / 2.0,
                                           (left[1]+right[1]) / 2.0,
                                           (left[2]+right[2]) / 2.0));

  extractor->setValue(gDataset);
  slice->setValue(request);

  // Connect the data flow
  slice->connectInput(extractor);
  slice->inherit(VISUS_BOUNDINGBOX_TYPEID,true);

  // Finally change the focus to the slice
  changeFocus(slice);
  //changeFocus(extractor);

  return slice;
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
    else if (start_end && !up) { // decrease the start resolution
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
    request.setStrides(start, end);
    
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
        ((pVisusOrthogonalSlice)gFocus)->translateRequest((x-gMouseX)/(float)win_width,(y-gMouseY)/(float)win_height);
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
  //fprintf(stderr,"key pressed \"%c\"\n",key);
  switch (key) {
  case 27: // ESC
  case 'x':
  case 'q':
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
      ((pVisusOrthogonalSlice)gFocus)->orientation(VISUS_ORIENTATION_XY);
    break;
  case '2':
    if ((gFocus != NULL) && (gFocus->type() == VISUS_ORTHO_SLICE)) {
      ((pVisusOrthogonalSlice)gFocus)->orientation(VISUS_ORIENTATION_XZ);
      fprintf(stderr,"Changing orientation\n");
    }
    break;
  case '3':
    if ((gFocus != NULL) && (gFocus->type() == VISUS_ORTHO_SLICE)) 
      ((pVisusOrthogonalSlice)gFocus)->orientation(VISUS_ORIENTATION_YZ);
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
  case 'c': // change colormap
    if ((gFocus != NULL) && gFocus->hasSharedValue(VisusSharedColorMap::sTypeIndex)) {
      gColorMap =(VisusColorMapType)((gColorMap + 1) % VISUS_CUSTOM_MAP);
      gFocus->setValue(construct_color_map(gColorMap));
      gFocus->markAsDirty();
    }
    break;
  case 'C': {  // change colormap
    VisusColorMap map;
    
    if ((gFocus != NULL) && gFocus->hasSharedValue(VisusSharedColorMap::sTypeIndex)) {
      gFocus->getValue(map);

      map.style((VisusColorMapStyle)((map.style()+1) % 3));
      gFocus->setValue(map);
      gFocus->markAsDirty();
      fprintf(stderr,"New colormap style %d\n",map.style());
    }
    break;
  }
  case '<': {
    VisusColorMap map;
    double low,high,tmp;
    
    if ((gFocus != NULL) && gFocus->hasSharedValue(VisusSharedColorMap::sTypeIndex)) {
      gFocus->getValue(map);
      map.getBounds(low,high);
      high -= 0.05 * (high - low);
      map.setBounds(low,high);
      gFocus->setValue(map);
      gFocus->markAsDirty();
    }
    break;
  }
  case '>': {
    VisusColorMap map;
    double low,high,tmp;
    
    if ((gFocus != NULL) && gFocus->hasSharedValue(VisusSharedColorMap::sTypeIndex)) {
      gFocus->getValue(map);
      map.getBounds(low,high);
      low += 0.05 * (high - low);
      map.setBounds(low,high);
      gFocus->setValue(map);
      gFocus->markAsDirty();
    }
    break;
  }
    
  
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

  VisusDataSource* data;

  // If no argument if given we create a default blob to view
  if (argc == 1) {

    std::vector<int> samples(3);
    std::vector<double> center(3);
    samples[0] = samples[1] = samples[2] = 100;
    center[0] = 0.5*(samples[0]-1);
    center[1] = 0.5*(samples[1]-1);
    center[2] = 0.5*(samples[2]-1);

    float *blob = new float[samples[0]*samples[1]*samples[2]];
    
    
    for (int i=0;i<samples[2];i++) {
      for (int j=0;j<samples[1];j++) {
        for (int k=0;k<samples[0];k++) {

          //c = (i%2 == 0) ^ (j%2 == 0) ^ (k%2 == 0);
          //blob[k + j*samples[0] + i*samples[0]*samples[1]] = c;
          
          blob[k + j*samples[0] + i*samples[0]*samples[1]] = 
            sqrt(pow(i-center[2],2) + pow(j-center[1],2) + pow(k-center[0],2));
        }
      }
    }

    VisusIncoreEncoder encoder(samples[0],samples[1],samples[2],1);
    encoder.domain(0,0,0,1.0*samples[0],1.0*samples[1],0.1*samples[2]);
    encoder.field(blob, "density", 0, blob[0]);
    std::string dataset_name = encoder.toString();

    gDataset = dataset_name.c_str();
    
    // Let's see whether we can open the data on its own
    data =  VisusDataSourceFactory::make(gDataset); 
    if (!data->isValid()) {
      fprintf(stderr,"Could not open data set \"%s\".\n", gDataset.c_str());
      exit(1);
    }
  }
  else { // If we have an argument we assume it is an idx file name
    
    char dataset_name[500];
    sprintf(dataset_name,"Idx:%s",argv[1]);
    gDataset = dataset_name;
    
    // Try to construct a fast loop data source
    data =  VisusDataSourceFactory::make(gDataset); 
    if (!data->isValid()) {
      fprintf(stderr,"Could not open data set \"%s\".\n",argv[1]);
      exit(1);
    }
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

  pVisusOrthogonalSlice slice1;
  slice1 = addSlice(gRoot);
  slice1->orientation(VISUS_ORIENTATION_XZ);
  
  pVisusOrthogonalSlice slice2;
  slice2 = addSlice(gRoot);
  slice2->orientation(VISUS_ORIENTATION_XY);
  
  
  pVisusOrthogonalSlice slice3;
  slice3 = addSlice(gRoot);
  slice3->orientation(VISUS_ORIENTATION_YZ);
  

  VisusFont font;  
  font.fontSize(5);

  pVisusDataProbe probe= VisusDataProbe::instantiate();
  probe->setValue(gDataset);
  probe->setValue(font);

  gRoot->attachSubTree(probe);
  //changeFocus(probe);
  

  /*
  // Add "Labels" To Axis
  pVisusTickMarks tickMarks;
  tickMarks = gObjectFactory.constructNode<VisusTickMarks>();
  tickMarks->legendText("Z-Axis");
  tickMarks->labelFont(font);
	tickMarks->direction(TM_Z_AXIS);
  gRoot->attachSubTree(tickMarks);

  tickMarks = gObjectFactory.constructNode<VisusTickMarks>();
  tickMarks->legendText("X-Axis");
  tickMarks->labelFont(font);
  gRoot->attachSubTree(tickMarks);

	tickMarks = gObjectFactory.constructNode<VisusTickMarks>();
  tickMarks->legendText("Y-Axis");
	tickMarks->direction(TM_Y_AXIS);
  tickMarks->labelFont(font);
  gRoot->attachSubTree(tickMarks);
  */

  glutMainLoop();
  return 1;
}

