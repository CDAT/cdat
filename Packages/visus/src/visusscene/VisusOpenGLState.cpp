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

#include "VisusOpenGLState.h"
#include <cstring>


const int    VisusOpenGLState::sDefaultWidth = 800;
const int    VisusOpenGLState::sDefaultHeight = 600;
const double VisusOpenGLState::sDefaultCameraPosition[3] = {0,0,30};
const double VisusOpenGLState::sDefaultView[3] = {0,0,-1};
const double VisusOpenGLState::sDefaultUp[3] = {0,1,0};
const double VisusOpenGLState::sDefaultViewVolume[6] = {-10,-10,0,10,10,100};
const GLint  VisusOpenGLState::sDefaultViewport[4] = {0,0,VisusOpenGLState::sDefaultWidth,VisusOpenGLState::sDefaultHeight};
const GLdouble VisusOpenGLState::sDefaultProjection[16] = {0.075, 0.0,   0.0, 0.0,
                                                           0.0,   0.1,   0.0, 0.0,
                                                           0.0,   0.0, -0.02, 0.0,
                                                           0.0,   0.0,  -0.4, 1.0};
VisusOpenGLState::VisusOpenGLState() 
{
  memcpy(mProjection,sDefaultProjection,16*sizeof(GLdouble));

  memcpy(mViewport,sDefaultViewport,4*sizeof(GLint));
}


VisusOpenGLState::VisusOpenGLState(const VisusOpenGLState& state)
{
  *this = state;
}

VisusOpenGLState& VisusOpenGLState::operator=(const VisusOpenGLState& state)
{
  memcpy(mProjection,state.mProjection,16*sizeof(GLdouble));
 
  memcpy(mViewport,state.mViewport,4*sizeof(GLint));

  return *this;
}

void VisusOpenGLState::fetchState()
{
  glGetIntegerv(GL_VIEWPORT,mViewport);
  glGetDoublev(GL_PROJECTION_MATRIX,mProjection);

  //printState();
}

void VisusOpenGLState::printState()
{
  fprintf(stderr,"viewport: %d %d %d %d\n",mViewport[0],mViewport[1],mViewport[2],mViewport[3]);
  fprintf(stderr,"projection:\n%f %f %f %f\n%f %f %f %f\n%f %f %f %f\n%f %f %f %f\n\n",
          mProjection[0],mProjection[4],mProjection[8],mProjection[12],
          mProjection[1],mProjection[5],mProjection[9],mProjection[13],
          mProjection[2],mProjection[6],mProjection[10],mProjection[14],
          mProjection[3],mProjection[7],mProjection[11],mProjection[15]);
   
}
