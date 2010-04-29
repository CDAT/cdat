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
#include <cstdio> 

#include "VisusGroup.h"
#include "VisusAssert.h"
#include "VisusXMLInterface.h"
#include "VisusDataSource.h"
#include "VisusDataSourceFactory.h"
#include "VisusIncoreEncoder.h"

#include <GL/gl.h>
#include <GL/glut.h>
#include <math.h>
#include <vector>

using namespace std;

//! Root of the scenegraph
pVisusGroup gRoot,gFocus;

int gMouseX,gMouseY;
int gPressed;
bool gMouseMotion = false;
int win_height = 600;
int win_width = 800;
static int mod = 0;


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
  static int lastRootKid = 0;
  static int i=0;

  mod = glutGetModifiers();
  //if (mod == GLUT_ACTIVE_SHIFT)

  switch (key) {
    case 27: 
      exit(0);
    case 'x':
      exit(0);
    case 'm': 
    {
      // Write XML data
      VisusXMLInterface xml;
      xml.write("loadXMLoutput.xml", gRoot);
      break;
    }
    case 'n':
    {
      if (gFocus->nrOfChildren() > 0) {
        gFocus = gFocus->child(0);
        std::cout << "Switching to first child of focus type(" << gFocus->type() << ")" << std::endl;
        std::cout.flush();
      }
      else {
        ++lastRootKid;
        if (lastRootKid < gRoot->nrOfChildren()) {
          gFocus = gRoot->child(lastRootKid);
          std::cout << "Switching to " << lastRootKid << " child of root" << std::endl;
          std::cout.flush();
        }
        else {
          std:: cout << "Switching to root of scene graph" << std::endl;
          std::cout.flush();
          gFocus = gRoot;
          lastRootKid = 0;
        }
      }
      break;
    }
    case '+':
    {
      if (gFocus->hasSharedValue(VisusSharedIsoValue::sTypeIndex)) 
      {
        VisusIsoValue value;
        gFocus->getValue(value);
        value.data(value.data() * 1.1);
        gFocus->setValue(value);
        std::cout << "Iso-Value updated to " << value.data() 
                  << " you won't see update in label since generic interface does not know of label"
                  << std::endl;
        std::cout.flush();
      }
      else {
        std::cout << "Focused VisusGroup does not have shared iso value" << std::endl;
        std::cout.flush();
      }
      break;
    }
    case '-':
    {
      if (gFocus->hasSharedValue(VisusSharedIsoValue::sTypeIndex)) 
      {
        VisusIsoValue value;
        gFocus->getValue(value);
        value.data(value.data() / 1.1);
        gFocus->setValue(value);
        std::cout << "Iso-Value updated to " << value.data() 
                  << " you won't see update in label since generic interface does not know of label"
                  << std::endl;
        std::cout.flush();
      }
      else {
        std::cout << "Focused VisusGroup does not have shared iso value" << std::endl;
        std::cout.flush();
      }
      break;
    }
    default:
      break;
  } 
}

void idle()
{
  redisplay(); 
}

void createData(const char* oldAddr)
{
  std::vector<int> samples(3);
  std::vector<double> center(3);
  samples[0] = samples[1] = samples[2] = 50;

  const int ndata = samples[0]*samples[1]*samples[2];
  float *blob = new float[ndata];
   
  const int maxidx = samples[0]*samples[1]*samples[2];
 
  for (int i=0;i<samples[2];i++) {
    for (int j=0;j<samples[1];j++) {
      for (int k=0;k<samples[0];k++) {
      	float value = i + j*j*100 + k*2;
      	//float value = i*i*i + sqrt(j*j * k*2.0);
        int idx = k + j*samples[0] + i*samples[0]*samples[1];	
        blob[maxidx - idx] = value;
      }
    }
  }
  VisusIncoreEncoder encoder(samples[0],samples[1],samples[2],1);
  encoder.domain(0,0,0,1.0*samples[0],1.0*samples[1],0.1*samples[2]);
  encoder.field(blob);
  std::string dataset_name = encoder.toString();

  char oldDD[1024];
  sprintf(oldDD, "Incore: 50 50 50 1 1 domain 0 0 0 50 50 5 { Unknown %s 30 4 }", oldAddr);

  VisusXMLInterface::registerDataDescription(oldDD, dataset_name);
}

int main(int argc,char *argv[])
{
  bool overrideData = false;
  char filename[1024] = "restart.xml";
  char oldAddr[1024] = "*";

  glutInit(&argc,argv);
  glutInitWindowSize(win_width,win_height);
  glutInitWindowPosition(200,200);

  glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH  | GLUT_MULTISAMPLE);
  glutCreateWindow("ViSUS 2.0 Load XML test");

  glutDisplayFunc(display);
  glutReshapeFunc(reshape);
  glutKeyboardFunc(keyboard);
  glutMouseFunc(mouse_callback);
  glutMotionFunc(motion);
  glutIdleFunc(idle);

  glInit();

  const char* filenameArg = "--filename=";
  const char* overrideDataArg= "--overrideData=";

  for (int i=0; i<argc; ++i)
  {
    if (!strncmp(argv[i], filenameArg, strlen(filenameArg))) {
      strcpy(filename, &(argv[i])[strlen(filenameArg)]); 
    }
    else if (!strncmp(argv[i], overrideDataArg, strlen(overrideDataArg))) {
      overrideData = true;
      strcpy(oldAddr, &(argv[i])[strlen(overrideDataArg)]);
    }
  }

  if (overrideData)
    createData(oldAddr);

  VisusXMLInterface xml;

  std::vector<std::string> data_descriptions;
  std::vector<std::string>::iterator it;
  
  data_descriptions = xml.extractDataDescriptions(filename);
  for (it=data_descriptions.begin();it!=data_descriptions.end();it++)
    fprintf(stderr,"Found data descriptiont \"%s\".\n",it->c_str());


  gRoot = xml.read(filename);
  gFocus = gRoot;

  if (gRoot == NULL)
    exit(0);

  glutMainLoop();
  return 1;
}

