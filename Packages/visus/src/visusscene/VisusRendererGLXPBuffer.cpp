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
  int attributes[] = { GLX_RGBA, GLX_DOUBLEBUFFER, None };
  
  mLastDrawable = None;
  mLastContext  = NULL;
  
  // Get a connection
  mDisplay = XOpenDisplay(0);
  if (mDisplay == NULL) {
	vwarning("Unable to create display connection to X");
	return;
  }
  vverbose("VisusRendererGLX::init:  Display(%x)\n", VISUS_RENDER_VERBOSE, mDisplay);

  if (! glXQueryVersion(mDisplay, &mMajorVersion, &mMinorVersion))
  {
    vwarning("Unable to get GLX version... setting to below 1.3");
    mMajorVersion = 1;
    mMinorVersion = 1;
  }
  if (mMajorVersion >=1 && mMinorVersion < 3) {
    std::ostringstream ss;
    ss << "Detected GLX version (" << mMajorVersion << "." << mMinorVersion
       << "). This code is for GLX1.3 and above." << std::endl;	  
    vwarning(ss.str().c_str());
  }
  vverbose("GLX Version (%d . %d)\n", VISUS_RENDER_VERBOSE, mMajorVersion, mMinorVersion);
  
  // Get Default Screen
  int screen = DefaultScreen(mDisplay);

  // Get Default Depth
  mDepth = DefaultDepth (mDisplay, screen);
  vverbose("VisusRendererGLX::init:  Screen(%d) Depth(%d)\n", VISUS_RENDER_VERBOSE, screen, mDepth);
  
  // Get an appropriate visual 
  mVisual = glXChooseVisual(mDisplay, screen, attributes);
  if (mVisual == NULL) {
    vwarning("Unable to create glx visual for offscreen rendering");
    return;
  }
  vverbose("VisusRendererGLX::init:  Visual(%x)\n", VISUS_RENDER_VERBOSE, mVisual);

  // Create a GLX context
  mContext = glXCreateContext(mDisplay, mVisual, 0, GL_TRUE);
  if (mContext == NULL) {
	vwarning("Unable to create glx context for offscreen rendering");
	return;
  }
  vverbose("VisusRendererGLX::init:  GLXContext(%d)\n", VISUS_RENDER_VERBOSE, mContext);

  // Create The GLX PBuffer
  int pbufferAttrs[] = { GLX_PBUFFER_WIDTH, mWidth, GLX_PBUFFER_HEIGHT, mHeight,
		  GLX_PRESERVED_CONTENTS, None
  };
  mPbuffer = glXCreatePbuffer(mDisplay, mConfig, pbufferAttrs);
  vverbose("VisusRendererGLX::init:  GLXPbuffer(%d)\n", VISUS_RENDER_VERBOSE, mPbuffer);

  vglerror();

  mValid = true;
}


VisusRenderer::~VisusRenderer()
{
  if (mDisplay != NULL) 
  {
	if (mVisual != NULL) 
	{
	  if (mContext != NULL) 
	  {
		glXDestroyContext(mDisplay, mContext);
		if (mValid)
		{
		  glXDestroyPbuffer(mDisplay, mPbuffer);	
		  //XFreePixmap(mDisplay, mXPixmap);
		}
	  }		
	}
	XCloseDisplay(mDisplay);
  }	
  vglerror();
}

bool VisusRenderer::makeCurrent()
{
  // Save Last Drawable/Context
  GLXContext context = glXGetCurrentContext();
  if (context != mContext) {
    mLastContext = context;
  }
  GLXDrawable drawable = glXGetCurrentDrawable();
  if (drawable != mPbuffer) {
	mLastDrawable = drawable;
  }
	
  // Make Pixmap Current Display
  if (! glXMakeContextCurrent(mDisplay, mPixmap, mPixmap, mContext)) {
    vwarning("Error occurred trying to set GLX pixmap as current drawable");
    return false;
  }
  vverbose("VisusRendererGLX::init:  Made pixmap current drawable\n", VISUS_RENDER_VERBOSE, NULL);
  vglerror();
  return true;
}

bool VisusRenderer::revertPrevious()
{
  if (! glXMakeContextCurrent(mDisplay, mLastDrawable, mLastDrawable, mLastContext)) {
	vwarning("Error occurred trying to current drawable as null");
	return false;
  }
  return true;
}


void VisusRenderer::getImage()
{	
  // Get The Image
  glReadPixels(0,0,mWidth,mHeight,GL_RGB,GL_UNSIGNED_BYTE,mImage);
  vglerror();
}
