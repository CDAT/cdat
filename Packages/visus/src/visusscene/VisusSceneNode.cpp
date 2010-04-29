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


#include "VisusSceneNode.h"
#include "VisusSharedCamera.h"

pVisusSceneNode VisusSceneNode::instantiate()
{
  return gObjectFactory.instantiate<VisusSceneNode>();
}


VisusSceneNode::VisusSceneNode() : VisusGroup(VISUS_SCENE_NODE)
{
  declareParameter<VisusSharedCamera>();
}


VisusSceneNode::~VisusSceneNode()
{
}

void VisusSceneNode::rotate(float x, float y)
{
  VisusBoundingBox box;
  VisusCamera camera;
  float rotation_center[4];

  // First accumulate the transformations from the root to this node
  VisusTransformation3D acc;
  accumulate3D(acc);

  y *= -1;
  //fprintf(stderr,"VisusGroup::rotate3D  x=%f  y=%f\n",x,y);

  getValue(camera);
  getValue(box);
  box.center(rotation_center);

  camera.rotate(acc,rotation_center,x,y);

  setValue(camera);
}

void VisusSceneNode::translate(float x, float y)
{
  VisusCamera camera;
  VisusOpenGLState state;

  // First accumulate the transformations from the root to this node
  VisusTransformation3D acc;
  accumulate3D(acc);

  y *= -1;
  //fprintf(stderr,"VisusGroup::rotate3D  x=%f  y=%f\n",x,y);

  getValue(camera);
  getValue(state);
  camera.translate(acc,state,x,y);
  setValue(camera);
}


void VisusSceneNode::scale(float x, float y)
{
  VisusBoundingBox box;
  VisusCamera camera;
  float scale_center[4];

  y *= -1;
  //fprintf(stderr,"VisusGroup::rotate3D  x=%f  y=%f\n",x,y);

  getValue(camera);
  getValue(box);
  box.center(scale_center);

  camera.scale(scale_center,x,y);

  setValue(camera);
}

void VisusSceneNode::rotateJoystick(float x, float y) 
{
  VisusCamera camera;

  getValue(camera);
  camera.turn(x,y);
  setValue(camera);
}
  
void VisusSceneNode::translateJoystick(float x, float y)
{
  VisusCamera camera;

  getValue(camera);
  camera.pan(x,y);
  setValue(camera);
}
  
void VisusSceneNode::scaleJoystick(float x, float y)
{
  VisusCamera camera;

  getValue(camera);
  camera.zoom(x,y);
  setValue(camera);
}

void VisusSceneNode::display3D(VisusTransformation3D model_view_3D)
{
  VisusCamera camera;

  getValue(camera);

  /*
  glMatrixMode(GL_MODELVIEW);  
  glPushMatrix();
  glLoadMatrixf(model_view_3D);
  */
  camera.setupOpenGL(model_view_3D);

  if (mDrawBoundingBox)
    displayBoundingBox();

  recurse(model_view_3D);
  
  glPopMatrix();
}


void VisusSceneNode::save(FILE* output)
{
  VisusTransformation3D m;
  VisusCamera camera;

  getValue(camera);

  //fprintf(output,"<VisusSceneNode>\n");
  camera.save(output);
  //fprintf(output,"<VisusSceneNode>\n");
}

int VisusSceneNode::load(FILE* input)
{
  VisusCamera camera;

  //char token[500];
  
  /*
  fscanf(input,"%s",token);
  
  if (strcmp(token,"<VisusSceneNode>") != 0) {
    vwarning("Incorrect token cannot read in scene node.");
    return 0;
  }
  */

  camera.load(input);

  /*
  if (strcmp(token,"<VisusSceneNode>") != 0) {
    vwarning("VisusSceneNode file input not consistent");
    return 0;
  }
  */
  setValue(camera);
  
  return 1;
}
