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


#include "VisusTextNode.h"
#include "VisusSharedFont.h"
#include "VisusSharedTransformation2D.h"


pVisusTextNode VisusTextNode::instantiate()
{
  return gObjectFactory.instantiate<VisusTextNode>();
}

VisusTextNode::VisusTextNode() : VisusGroup(VISUS_TEXT_NODE), VisusText()
{
  declareParameter<VisusSharedFont>();
  declareParameter<VisusSharedTransformation2D>();

  mBoundingBoxMode = VISUS_2D_RENDER;
}

VisusTextNode::VisusTextNode(const char* st) : VisusGroup(), VisusText(st)
{
  declareParameter<VisusSharedFont>();
  declareParameter<VisusSharedTransformation2D>();

  mNodeType = VISUS_TEXT_NODE;
}

void VisusTextNode::setPosition(float x, float y)
{
  getValue(mMatrix);
  
  mMatrix[6] = x;
  mMatrix[7] = y;

  setValue(mMatrix);
}


void VisusTextNode::display2D(VisusTransformation2D model_view_2D)
{
  getValue<VisusSharedTransformation2D>(mMatrix);
  getValue<VisusSharedFont>(mFont);


  glMatrixMode(GL_MODELVIEW);
  
  glPushMatrix();
  glLoadIdentity();

  
  if (!frozen())
    model_view_2D *= mMatrix;
  else
    model_view_2D = mFrozen2D;

  glTranslatef(model_view_2D[6],model_view_2D[7],0);
  render(600);

  glPopMatrix();

  recurse(model_view_2D);
}

