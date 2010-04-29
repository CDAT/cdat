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


#include "VisusScreenShot.h"
#include <stdio.h>
//#include <GL/glx.h>


VisusScreenShot::VisusScreenShot(int width, int height)
{
  mWidth = width;
  mHeight = height;

  if (mWidth*mHeight <= 0) {
    fprintf(stderr,"Error VisusVisusScreenShot() invalid screen size\n");
    mBuffer = NULL;
  }
  else
    mBuffer = new GLubyte[4*width*height];
}


VisusScreenShot::~VisusScreenShot() 
{
  if (mBuffer != NULL)
    delete[] mBuffer;
}


void VisusScreenShot::dumpScreen(const char *filename)
{
  FILE *output = NULL;
  
  output = fopen(filename,"wb");

  if (output == NULL) {
    fprintf(stderr,"Error VisusScreenShot::dumpScreen could not open file \"%s\"\n\n",filename);
    return;
  }


  glReadBuffer(GL_BACK);
  //glReadBuffer(GL_AUX0);
  glReadPixels(0,0,mWidth,mHeight,GL_RGBA,GL_UNSIGNED_BYTE,mBuffer);
  
  GLubyte *mirror = new GLubyte[4*mWidth*mHeight];
  
  
  fprintf(output, "P6\n");
  fprintf(output, "%d %d\n",mWidth,mHeight); 
  fprintf(output, "255\n");
  
  for (int i=0;i<4*mWidth*mHeight;i++) {
    //fprintf(stderr,"i=%d     %d\n",i,i);fflush(stderr);//(mHeight-1-i/(3*mWidth))*3*mWidth + i%(3*mWidth));
    mirror[(mHeight-1-i/(4*mWidth))*4*mWidth + i%(4*mWidth)] = mBuffer[i];
  }
  
  for (int i=0;i<mWidth*mHeight;i++)
    fwrite(mirror+4*i,1,3,output);
  //  fwrite(mirror, 1, 4*mWidth*mHeight, output);

  while (fclose(output));
  

  delete[] mirror;
}

void VisusScreenShot::setupGLX_1_2()
{
  /*
  int render_attributes[] = {GLX_RGBA, None };

   int major,minor;
  
  glXQueryVersion(mDisplay,&major,&minor);
  
  Pixmap pix = XCreatePixmap(mDisplay,glXGetCurrentDrawable(),
                             mWidth,mHeight,
                             24);
  
  GLXFBConfig *fbConfigs;
  int numReturned;

  //fbConfigs = glXChooseFBConfig(x_connection,DefaultScreen(x_connection),
  //                            bufferAttributes,&numReturned);
  fbConfigs = glXGetFBConfigs(mDisplay,DefaultScreen(mDisplay),
                              &numReturned);
  if (fbConfigs == NULL) {
    fprintf(stderr,"Could not find frame buffer config\n");
    return;
  }

  GLXPixmap buffer = glXCreatePixmap(mDisplay, fbConfigs[0], 
                                     pix,render_attributes);

  //glXMakeContextCurrent(mDisplay,buffer,buffer,(GLXContext)glcanvas->getContext());
  */
}

