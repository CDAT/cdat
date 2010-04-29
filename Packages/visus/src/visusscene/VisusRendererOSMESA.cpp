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


#include <fstream>
#include <iostream>
#include <sstream>

#include "VisusRenderer.h"

void VisusRenderer::init()
{
  mLastContext = 0;
  
  mContext = OSMesaCreateContextExt( OSMESA_RGB, 16, 0, 0, NULL );
  if (! mContext) 
  {
	vwarning("Failed to create OSMESA context");
	return;
  }
	
  mValid = true;
}


VisusRenderer::~VisusRenderer()
{
  if (mContext) {
	OSMesaDestroyContext(mContext);
	mContext = NULL;
  }
  vglerror();
}

bool VisusRenderer::makeCurrent()
{
  OSMesaContext context = OSMesaGetCurrentContext();
  if (context != mContext)
  {
    mLastContext = context;
  }
	
  if (! OSMesaMakeCurrent(mContext, mImage, GL_UNSIGNED_BYTE, mWidth, mHeight )) {
	vwarning("Failed to make OSMESA context current");
	return false;
  }

  {
      int z, s, a;
      glGetIntegerv(GL_DEPTH_BITS, &z);
      glGetIntegerv(GL_STENCIL_BITS, &s);
      glGetIntegerv(GL_ACCUM_RED_BITS, &a);
      vverbose("Depth=%d Stencil=%d Accum=%d\n", VISUS_RENDER_VERBOSE, z, s, a);
   }

  vglerror();
  return true;
}

bool VisusRenderer::revertPrevious()
{
  if (mLastContext == 0)
	  return true;
  
  return true;
}


void VisusRenderer::getImage()
{	
  // Don't need to do anything; everything already in mImage
}
