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

#include "VisusRenderer.h"

VisusRenderer::VisusRenderer(const int width, const int height):
  mWidth(width), mHeight(height), mSize(width*height), mValid(false)
{	
  mPerPixel = 3;	
	
  // Create Storage For Image
  mImage = new unsigned char[mSize*mPerPixel];
  for (int i=0; i<mSize; ++i) {
	  mImage[i] = 0;
  }
  vverbose("VisusRenderer::ctor:  Width(%d) Height(%d)\n", 500, mWidth, mHeight);

  init();
}


bool VisusRenderer::render(pVisusGroup root)
{
  if (! makeCurrent())
    return false;

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  root->display();
  glFlush();
  glFinish();

  // Get The Image
  getImage();

  if (! revertPrevious())
    return false;

  return true;
}


bool VisusRenderer::write(const char* file) const
{
  if (! mValid) 
  {
    vwarning("attempting to call write when VisusRenderer was not initialized correctly");
    return false;  
  }

  std::ofstream out(file, std::ios::out|std::ios::binary);
  if (!out) {
    std::cerr << "Error: cannot open " << file << std::endl;
    return false;
  }

  // Flip The Image
  unsigned char* dataFlip = new unsigned char[mSize*mPerPixel];
  for (int i=0; i<mWidth; ++i) {
    for (int j=0; j<mHeight; ++j) {
      for (int k=0; k<mPerPixel; ++k) {
        dataFlip[mPerPixel*(j*mWidth + i) + k] = mImage[mPerPixel*((mHeight-1-j)*mWidth + i) + k];
      }
    }
  }

  int colors=255;  // Color depth

  // Write The File
  out << "P6" << std::endl
      << mWidth << " " << mHeight << std::endl 
      << colors << std::endl;
    
  out.write((char*)dataFlip, mSize*mPerPixel);
  out.close();
  
  return true;
}

bool VisusRenderer::render(pVisusGroup root, const char* file)
{
  if (! render(root))
    return false;

  return write(file);
}
