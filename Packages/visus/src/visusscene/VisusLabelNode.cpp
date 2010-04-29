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

#include "xmlParser.h"

#include "VisusLabelNode.h"
#include "VisusSharedFont.h"
#include "VisusSharedTransformation2D.h"


pVisusLabelNode VisusLabelNode::instantiate()
{
  return gObjectFactory.instantiate<VisusLabelNode>();
}

VisusLabelNode::VisusLabelNode() : VisusGroup(VISUS_LABEL_NODE)
{
  declareParameter<VisusSharedFont>();
  declareParameter<VisusSharedTransformation2D>();

  mBoundingBoxMode = VISUS_2D_RENDER;
}

VisusLabelNode::VisusLabelNode(const char* st) : VisusGroup(VISUS_LABEL_NODE), mLabel(st)
{
  declareParameter<VisusSharedFont>();
  declareParameter<VisusSharedTransformation2D>();

  mBoundingBoxMode = VISUS_2D_RENDER;
}

void VisusLabelNode::text(const char* st)
{
  mLabel.text(st);
  markAsDirty();
}

void VisusLabelNode::position(float x, float y)
{
  VisusTransformation2D local;

  getValue(local);
  
  local[6] = x;
  local[7] = y;

  setValue(local);
}

void VisusLabelNode::display2D(VisusTransformation2D model_view_2D)
{
  VisusFont font;
  VisusTransformation2D local;
  VisusOpenGLState state;
  VisusBoundingBox bbox;

  getValue<VisusSharedTransformation2D>(local);
  getValue<VisusSharedFont>(font);
  getValue(state);

  glMatrixMode(GL_MODELVIEW);
  
  glPushMatrix();
  glLoadIdentity();
  
  model_view_2D *= local;

  //glLoadMatrixf(model_view_2D);
  glTranslatef(model_view_2D[6],model_view_2D[7],0);

  mLabel.font(font);

  bbox = mLabel.bbox();
  bbox.scale(1.05);
  setValue(bbox,false);

  if (drawBoundingBox()) 
    displayBoundingBox();
    

  mLabel.render(state.viewport()[3]);

  glPopMatrix();
  
  recurse(model_view_2D);
}


void VisusLabelNode::toXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName()))
    vwarning("VisusLabelNode did not receive top level node");

  mLabel.toXML(node);
}

bool VisusLabelNode::fromXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusLabelNode did not receive top level node");
    return false;
  }

  XMLNode child = node.getChildNode(VisusText::XML_TAG);
  if (! mLabel.fromXML(child)) {
    vwarning("unable to initialize text label from xml");
    return false;
  }

  return true;
}
