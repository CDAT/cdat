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
  #include <windows.h>
#endif

#include <glew.h>
#include <string>
#include <xmlParser.h>
#include <iostream>
using namespace std;

#include "VisusFont.h"
#include "VisusAssert.h"

#ifndef VISUS_DISABLE_FTGL

  #include <FTGL/FTGLExtrdFont.h>
  #include <FTGL/FTGLOutlineFont.h>
  #include <FTGL/FTGLPolygonFont.h>
  #include <FTGL/FTGLTextureFont.h>
  #include <FTGL/FTGLPixmapFont.h>
  #include <FTGL/FTGLBitmapFont.h>

#endif

const char*            VisusFont::XML_TAG = "VisusFont";
const char*            VisusFont::DEFAULT_FONT_FILE = VISUS_FONT_FILE;
const VisusFontScaling VisusFont::DEFAULT_FONT_SCALING = VISUS_FONT_RELATIVE;
const VISUS_FONT_STYLE VisusFont::DEFAULT_FONT_STYLE = VISUS_PIXMAP;
const unsigned char    VisusFont::ALL_STYLES_VALID = 63;
const int              VisusFont::DEFAULT_FONT_SIZE = 18;
const int              VisusFont::FONT_SIZE_MULTIPLIER = 1;
const unsigned char    VisusFont::DEFAULT_FONT_COLOR[4] = { 219, 219, 112, 255 };
const float            VisusFont::DEFAULT_FONT_DEPTH = 1;



// Default constructor uses the above defaults
VisusFont::VisusFont()
{
  mFontScaling = DEFAULT_FONT_SCALING;
  mFontStyle = DEFAULT_FONT_STYLE;
  mFontSize = DEFAULT_FONT_SIZE;
  mFontRenderScale = 1;
  memcpy(mFontColor,DEFAULT_FONT_COLOR,4);

  vassert(strlen(DEFAULT_FONT_FILE) < 1024);
  strcpy(mFontFile,DEFAULT_FONT_FILE);
  mFontDepth = DEFAULT_FONT_DEPTH;
  
  mFontRenderer = NULL;
  mFontRenderer = fontFactory("VisusFont()");
}

VisusFont::VisusFont(const VisusFont& font):
	mFontRenderer(NULL)
{
  *this = font;
}

VisusFont::~VisusFont()
{
  if(mFontStyle != VISUS_TEXTURE)
    if (mFontRenderer != NULL) 
      delete mFontRenderer;
}

VisusFont& VisusFont::operator=(const VisusFont& font)
{
  if (&font == this) {
    vwarning("In VisusFont, attempt to assign self to self");
    return *this;
  }
	
  mFontStyle = font.mFontStyle;
  mFontSize = font.mFontSize;
  mFontDepth = font.mFontDepth;

  strcpy(mFontFile,font.mFontFile);

  memcpy(mFontColor,font.mFontColor,4*sizeof(unsigned char));
  if(mFontStyle != VISUS_TEXTURE)
    if (mFontRenderer !=NULL)
      delete mFontRenderer;
  mFontRenderer = fontFactory("operator=");

  return *this;
}


void VisusFont::fontStyle(VISUS_FONT_STYLE style)
{
  mFontStyle = style;

  if (mFontRenderer != NULL) 
    delete mFontRenderer;

  mFontRenderer = fontFactory("fontStyle()");
  fontUpdate();
}

void VisusFont::fontSize(int size, int dpi)
{
  //fprintf(stderr,"Setting font size to %d\n",size);
  
  mFontSize = size;
  fontRenderScale(mFontRenderScale);
  fontUpdate();
}

void VisusFont::fontDepth(float depth)
{
  mFontDepth = depth;

  if (mFontRenderer != NULL)
    mFontRenderer->Depth(mFontDepth);

  fontUpdate();
}

void VisusFont::fontFile(const char *file)
{
  strcpy(mFontFile,file);  

  if (mFontRenderer != NULL) 
    delete mFontRenderer;

  mFontRenderer = fontFactory("fontFile()");
  fontUpdate();
}

void VisusFont::fontColor(unsigned char color[4])
{
  memcpy(mFontColor,color,4);
}

void VisusFont::fontColor(unsigned char r,unsigned char g, unsigned char b,
               unsigned char alpha)
{
  mFontColor[0] = r;
  mFontColor[1] = g;
  mFontColor[2] = b;
  mFontColor[3] = alpha; 
}

bool VisusFont::isValidStyle(VISUS_FONT_STYLE style) const 
{
  return ((unsigned char)(1 << style) & ALL_STYLES_VALID);
}


void VisusFont::bbox(const char *st, float bb[6])
{
  if (mFontRenderer != NULL) {
      mFontRenderer->BBox(st,bb[0],bb[1],bb[2],bb[3],bb[4],bb[5]);

      // For some reason FTGL returns an incorrect bounding box in the
      // z-axis for extruded fonts. This might be a bug or a
      // deliberate design choice but in either case we must correct
      // the box prior to rendering. It appears that the fix is to
      // multiply the max z-coordinate with (-1). 
      if (mFontStyle == VISUS_EXTRUDE)
        bb[5] *= -1;

  } 
  else {
    fprintf(stderr,"String %s\n",st);
    vwarning("VisusFont::BBox no font created");
  }
}

void  VisusFont::fontRenderScale(float scale)
{
  mFontRenderScale = scale;  
  
  if (mFontRenderer != NULL) 
    mFontRenderer->FaceSize(int(scale*mFontSize*FONT_SIZE_MULTIPLIER+0.5));
}



void VisusFont::render(const char *st)
{
  //std::cout << "VisusFont::render" << std::endl;
  //std::cout << "style: " << mFontStyle << std::endl;
  //std::cout << "mFontColor: " << (int)mFontColor[0] << " "
  //          << (int)mFontColor[1] << " " << (int)mFontColor[2] << " " << (int)mFontColor[3]<< std::endl;
  if (mFontRenderer != NULL) {
    
    //  std::cout << "set the color" << std::endl;

    glColor4ubv(mFontColor);

    switch(mFontStyle) {
    case VISUS_BITMAP:
    case VISUS_PIXMAP:
      glDisable(GL_LIGHTING);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glRasterPos2i(0,0);
      break;
    case VISUS_OUTLINE:
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
      break;
    case VISUS_POLYGON:
      glEnable( GL_TEXTURE_2D);
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      break;
    case VISUS_EXTRUDE:
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      //glEnable( GL_DEPTH_TEST);
      glEnable( GL_TEXTURE_2D);
      break;
    case VISUS_TEXTURE:
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glEnable( GL_TEXTURE_2D);
      glDisable(GL_LIGHTING);
      //glDisable( GL_DEPTH_TEST);
      glNormal3f( 0.0, 0.0, 1.0);
      break;
      
    }
    mFontRenderer->Render(st);
    
    switch(mFontStyle) {
    case VISUS_BITMAP:
    case VISUS_PIXMAP:
      glEnable(GL_LIGHTING);
    case VISUS_OUTLINE:
      break;
    case VISUS_POLYGON:
    case VISUS_EXTRUDE:
    case VISUS_TEXTURE:
      //glEnable( GL_DEPTH_TEST);
      glDisable( GL_TEXTURE_2D);
      glEnable(GL_LIGHTING);
      break;
      
    }
  }
}

VISUS_FTFont *VisusFont::fontFactory(const char *caller)
{
  VISUS_FTFont *font = NULL;

#ifndef VISUS_NO_FTGL

  switch (mFontStyle) {
    
  case VISUS_BITMAP:
    font = new FTGLBitmapFont(mFontFile);
    break;
  case VISUS_PIXMAP:
    font = new FTGLPixmapFont(mFontFile);
    break;
  case VISUS_OUTLINE:
    font = new FTGLOutlineFont(mFontFile);
    break;
  case VISUS_POLYGON:
    font = new FTGLPolygonFont(mFontFile);
    break;
  case VISUS_EXTRUDE:
    font = new FTGLExtrdFont(mFontFile);
    break;
  case VISUS_TEXTURE:
    font = new FTGLTextureFont(mFontFile);
    break;
  }

  if (font->Error()) {
    fprintf(stderr,"Error VisusFont::%s: Could not open font\n\
\"%s\"\nNo text will be rendered\n",caller,mFontFile);
    return NULL;
  }

  font->FaceSize(mFontSize*FONT_SIZE_MULTIPLIER);
  font->Depth(mFontDepth);
  font->CharMap(ft_encoding_unicode);

#endif

  return font;
}

void VisusFont::toXML(XMLNode& parent) const
{
  XMLNode font = parent.addChild(XML_TAG);
  font.addAttribute("style", mFontStyle);
  font.addAttribute("scaleType", mFontScaling);
  font.addAttribute("size", mFontSize);
  font.addAttribute("scale", mFontRenderScale);
  font.addAttribute("depth", mFontDepth);
  font.addAttribute("file", mFontFile);
  font.addAttribute("r", mFontColor[0]);
  font.addAttribute("g", mFontColor[1]);
  font.addAttribute("b", mFontColor[2]);
  font.addAttribute("a", mFontColor[3]);
}

bool VisusFont::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) 
  {
    vwarning("VisusFont did not receive top level node");
    vverbose("VisusFont did not receive top level node", VISUS_XML_VERBOSE);
    return false;
  }

  mFontStyle = (VISUS_FONT_STYLE)xmltoi(node.getAttribute("style"), mFontStyle);
  mFontScaling = (VisusFontScaling)xmltoi(node.getAttribute("scaleType"), mFontScaling);
  mFontSize = xmltoi(node.getAttribute("size"), mFontSize);
  mFontRenderScale = xmltof(node.getAttribute("scale"), mFontRenderScale);
  mFontDepth = xmltof(node.getAttribute("depth"), mFontDepth);
  strcpy(mFontFile, node.getAttribute("file"));

  mFontColor[0] = xmltoi(node.getAttribute("r"), 255);
  mFontColor[1] = xmltoi(node.getAttribute("g"), 255);
  mFontColor[2] = xmltoi(node.getAttribute("b"), 255);
  mFontColor[3] = xmltoi(node.getAttribute("a"), 255);
  return true;
}
