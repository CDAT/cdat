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


#include "VisusGuiBase.h"

#include <cstdio>
#include <GL/gl.h>
#include <GL/glu.h>

#include "VisusSceneNode.h"
#include "VisusSharedDataRequest.h"

#include <iostream>
VisusGuiBase::VisusGuiBase(int width, int height) : mWindowWidth(width), mWindowHeight(height), 
                                                    mPosX(0),mPosY(0),mMouseButton(0)
{
  mFocusLabel = VisusLabelNode::instantiate();
  mFocusLabel->text("No Focus");
  mFocusLabel->position(-0.95*width/height,-0.95);
  VisusFont font;
  font.fontSize(4);
  mFocusLabel->setValue(font);
  mFocusBBoxFlag = false;
}

VisusGuiBase::~VisusGuiBase()
{
}


void VisusGuiBase::initOpenGL()
{
  float light1_ambient[4]  = { 0.1, 0.1, 0.1, 1.0 };
  float light1_diffuse[4]  = { 0.8, 0.8, 0.8, 1.0 };
  float light1_specular[4] = { 1.0, 1.0, 1.0, 1.0 };
  float light1_position[4] = { -2.0, 1.0, 4.0, 0.0 };
  glLightfv(GL_LIGHT1, GL_AMBIENT,  light1_ambient);
  glLightfv(GL_LIGHT1, GL_DIFFUSE,  light1_diffuse);
  glLightfv(GL_LIGHT1, GL_SPECULAR, light1_specular);
  glLightfv(GL_LIGHT1, GL_POSITION, light1_position);

  glEnable(GL_LIGHT1);
  glEnable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_COLOR_MATERIAL);
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
  glShadeModel(GL_SMOOTH);
 
  glClearColor(0,0,0,0);
   
  glMatrixMode( GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-10*mWindowWidth/(float)mWindowHeight,10*mWindowWidth/(float)mWindowHeight,-10,10,0,100);
  gluLookAt(0,0,30,0,0,29,0,1,0);
   
  mOpenGLState.fetchState();
  if (mRoot != NULL)
    mRoot->setValue(mOpenGLState);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

int VisusGuiBase::setRoot(pVisusGroup root)
{
  // If the user simply want to clear the graph 
  if (root == NULL) {
    mRoot = root; // we do so an exit (no you can't write mRoot = NULL)
    return 1;
  }

  mRoot = root;
  
  setFocus(root);
  
  // If the node is not a scene node we must fit it into our standard
  // global coordinate system
  if (mRoot->type() != VISUS_SCENE_NODE) {
    VisusBoundingBox box;
    
    // First we must determine whether the root has a non-trivial valid
    // bounding box
    mRoot->getValue(box);

    // If this bounding box is not valid 
    if (!box.valid()) {
      vwarning("Scene graph root has invalid bounding box the view transformation might be invalid.");
      return 0; 
    }
    
    // The standard view is an orthogonal projection showing the cube
    // [-10,-10,-10]x[10,10,10] at a reasonable distance. We want the
    // data to appear at [-8,-8,-8]x[8,8,8]
    box[0] = box[1] = box[2] = -8;
    box[3] = box[4] = box[5] = 8;

    mRoot->mapToWorldBox(box);
  }

  return 1;
}    

int VisusGuiBase::setFocus(pVisusGroup focus)
{
  // If the user simply want to clear the graph 
  if (focus == NULL) {
    vwarning("Cannot set NULL as focus");
    return 0;
  }

  if (mFocus != NULL) {
    mFocus->drawBoundingBox(mFocusBBoxFlag);
    mFocus->boundingBoxColor(VisusColor(1,1,1));
  }

  mFocus = focus;

  if (mFocus != NULL) {
    mFocusLabel->text(mFocus->infoString());
    mFocusBBoxFlag = mFocus->drawBoundingBox();
    mFocus->drawBoundingBox(true);
    mFocus->boundingBoxColor(VisusColor(1,0,0));
  }

  return 1;
}

void VisusGuiBase::display()
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();

  //fprintf(stderr,"Drawing\n");
  glColor3f(1,1,1);

  VisusOpenGLState state;
  state.fetchState();
  mRoot->setValue(state,false);

  mRoot->display();

  if (modifier() & VISUS_CAPS)
    mFocusLabel->display();

  swapBuffers();
}

void VisusGuiBase::mouse(int button, int state, int x, int y)
{
  //fprintf(stderr,"mouse button %d\n",button);

  if (state == VISUS_MOUSE_DOWN) {
    // record which button has been pressed by setting the
    // corresponding bit of the gMouseButton variable
    mMouseButton |= 1 << button; 
    mPosX = x;
    mPosY = y;
  }
  else {
    mMouseButton &= ~(1 << button);
  }
  
  redisplay();
}


void VisusGuiBase::motion(int x, int y)
{
  // If no mouse button is pressed
  if (mMouseButton == 0) 
    return;

  float dx,dy;

  dx = (x - mPosX) / (float)mWindowWidth;
  dy = (y - mPosY) / (float)mWindowHeight;


  if (modifier() & VISUS_SHIFT) { //If shift is pressed
    if (isPressed(VISUS_MIDDLE_MOUSE)) {
      if ((mFocus != NULL) // If we have a valid focus
          && mFocus->hasSharedValue(VisusSharedDataRequest::sTypeIndex)) { // and the node contains a request
      
        // We want to modify the request rather than the node
        VisusDataRequest request;
        mFocus->getValue(request);
        
        VisusTransformation3D acc;
        mFocus->accumulate3D(acc);
        request.shift(acc,mOpenGLState,dx,-dy);
    
      
        mFocus->setValue(request);
      }   
    }
    else if (isPressed(VISUS_LEFT_MOUSE)) {
      mRoot->rotate(dx,dy);
    }
    else if (isPressed(VISUS_RIGHT_MOUSE)) {
      if (mRoot->type() == VISUS_SCENE_NODE) { // If the node is a scenenode 
        // We prefer tightening the view frustum rather than scaling the
        // wold coordinate system
        ((pVisusSceneNode)mRoot)->scaleJoystick(dx,dy);   
      }
      else {
        mRoot->scale(dx,dy);
      }
    }
  }
  else { // If shift is not pressed
    if (isPressed(VISUS_LEFT_MOUSE)) {
      if (mFocus != NULL) 
        mFocus->rotate(dx,dy);
    }
    else if (isPressed(VISUS_MIDDLE_MOUSE)) {
      if (mFocus != NULL)
        mFocus->translate(dx,dy);
    }
    else if (isPressed(VISUS_RIGHT_MOUSE)) {
      if (mFocus != NULL) {
        if (mFocus->type() == VISUS_SCENE_NODE) { // If the node is a scenenode 
          // We prefer tightening the view frustum rather than scaling the
          // wold coordinate system
          ((pVisusSceneNode)mFocus)->scaleJoystick(dx,dy);   
        }
        else {
          mFocus->scale(dx,dy);
        }
      }
    }
  }

  mPosX = x;
  mPosY = y;

  redisplay();
}

void VisusGuiBase::keyboard(unsigned char key, int x, int y)
{
  //fprintf(stderr,"Key pressed %c\n",key);
  switch (key) {
    case 9: // tab
      if (mFocus != NULL) {
        if (modifier() & VISUS_SHIFT) 
          setFocus(mFocus->nextNode(VISUS_EXTRACTOR_NODE));
        else
          setFocus(mFocus->nextNode(VISUS_DISPLAY_NODE));
      }
      else {
        setFocus(mRoot);
      }
        
      break;
  case 'f':
    mFocus->freeze();
    break;
  case 'S':
  case '=': {
    VisusGlobalTime t;
    if ((mRoot != NULL) && (mRoot->hasSharedValue(VISUS_GLOBALTIME_TYPEID))) {
      mRoot->getValue(t);
      t.inc();
      mRoot->setValue(t);
      fprintf(stderr,"Time %s\n",t.time().toString().c_str());
    }
    break;
  }
  case 'Q':
  case '-': {
    VisusGlobalTime t;
    if ((mRoot != NULL) && (mRoot->hasSharedValue(VISUS_GLOBALTIME_TYPEID))) {
      mRoot->getValue(t);
      t.dec();
      mRoot->setValue(t);
      fprintf(stderr,"Time %s\n",t.time().toString().c_str());
    }
    break;
  }
  default:
    break;
  }

  redisplay();
}
   

bool VisusGuiBase::isPressed(VisusMouseButton button)
{
  return ((mMouseButton & (1 << button)) != 0);
}
