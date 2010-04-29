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


#include <cstring>
#include <sstream>

#include "xmlParser.h"

#include "VisusMath.h"
#include "VisusColorMap.h"
#include "VisusDefinitions.h"
#include "VisusAssert.h"
#include "VisusXMLInterface.h"
#include "VisusDefaultColorMaps.h"

const char* VisusColorMap::XML_TAG = "VisusColorMap";

VisusColorMap::VisusColorMap(int resolution, VisusColorMapType t) 
  : VisusVersioned(0), mType(t), mInitialized(false)
{
  int count = 0;
  
  if (resolution < 1) {
    vwarning("Color map resolution must be >0. Defaulting to 1.");
    resolution = 1;
  }

  while (resolution > 1) {
  resolution = resolution >> 1;
  count++;
  } 
  resolution = 1 << count;
  
  mResolution = resolution;
  mMap = new float[4*(mResolution+2)];

  for (int i=1;i<mResolution+1;i++) {
    mMap[4*i] = mMap[4*i+1] = mMap[4*i+2] = (i-1) / (float)(mResolution-1.0);
    mMap[4*i+3] = 1;
  }

  mMap[0] = mMap[1] = mMap[2] = 0;
  mMap[3] = 1;

  mMap[4*(mResolution+1)] =  mMap[4*(mResolution+1)+1] = mMap[4*(mResolution+1)+2] = 1;
  mMap[4*(mResolution+1)+3] = 1;

  mTexId = 0;

  style(VISUS_LINEAR_MAP);
}

VisusColorMap::VisusColorMap(const VisusColorMap& map):
 	mMap(NULL), mResolution(0), mTexId(0)
 {
 	*this = map;
 }


VisusColorMap::~VisusColorMap()
{
  if (mTexId != 0) 
    glDeleteTextures(1,&mTexId);
  mTexId = 0;
  
  delete[] mMap;
}

VisusColorMap& VisusColorMap::operator=(const VisusColorMap& map)
{
  mType = map.mType;
  mStyle = map.mStyle;
  mInitialized = map.mInitialized;
  mMapFunction = map.mMapFunction;
  mMapAnchor = map.mMapAnchor;
  mMapScale = map.mMapScale;
  mLow = map.mLow;
  mHigh = map.mHigh;

  // We do not copy the version number but instead increase our own
  incVersion();

  // If the resolution is different
  if (map.resolution() != mResolution) {
    
    // We will need to upload a new texture so we must first clean up
    // the old one
    if (mTexId != 0) { // If there is an old texture on the graphics card
      glDeleteTextures(1,&mTexId); // Remove it
      mTexId = 0;
    }
    mResolution = map.resolution();

    // allocate the appropriate memory
    delete[] mMap;
    mMap = new float[4*(mResolution+2)];

    // Copy the data
    memcpy(mMap,map.mMap,4*(mResolution+2)*sizeof(float));

    // Note that the mTexId=0 will cause a new texture to be created
    // and uploaded later
  }
  else if (memcmp(mMap,map.mMap,4*(mResolution+2)*sizeof(float)) != 0) {
    // If the resolution is the same but the content has changed

    // Copy the new data
    memcpy(mMap,map.mMap,4*(mResolution+2)*sizeof(float));

    // Re-upload the texture to the graphics card
    if (mTexId != 0) {
      glBindTexture(GL_TEXTURE_1D, mTexId);
      glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,mResolution,0,GL_RGBA,
                   GL_FLOAT,mMap+4);
    }
  }    
  

  return *this;
}

int VisusColorMap::getColor(float f, unsigned char* color) const
{
  int i = (this->*mMapFunction)(f);
  
  vassert((i >= 0) && (i < 4*(mResolution+2)));

  color[0] = 255*mMap[i];
  color[1] = 255*mMap[i+1];
  color[2] = 255*mMap[i+2];
  color[3] = 255*mMap[i+3];

  return 1;
}


int VisusColorMap::getColor(float f, float* color) const 
{
  int i = (this->*mMapFunction)(f);
  
  vassert((i >= 0) && (i < 4*(mResolution+2)));

  color[0] = mMap[i++];
  color[1] = mMap[i++];
  color[2] = mMap[i++];
  color[3] = mMap[i];

  return 1;
}


int VisusColorMap::getFloorColor(unsigned char* color) const
{
  color[0] = 255*mMap[0];
  color[1] = 255*mMap[1];
  color[2] = 255*mMap[2];
  color[3] = 255*mMap[3];

  return 1;
}


int VisusColorMap::getFloorColor(float* color) const
{
  color[0] = mMap[0];
  color[1] = mMap[1];
  color[2] = mMap[2];
  color[3] = mMap[3];

  return 1;
}

int VisusColorMap::getCeilingColor(unsigned char* color) const
{
  color[0] = 255*mMap[4*(mResolution+1)];
  color[1] = 255*mMap[4*(mResolution+1) + 1];
  color[2] = 255*mMap[4*(mResolution+1) + 2];
  color[3] = 255*mMap[4*(mResolution+1) + 3];

  return 1;
}

int VisusColorMap::getCeilingColor(float* color) const
{
  color[0] = mMap[4*(mResolution+1)];
  color[1] = mMap[4*(mResolution+1) + 1];
  color[2] = mMap[4*(mResolution+1) + 2];
  color[3] = mMap[4*(mResolution+1) + 3];

  return 1;
}

int VisusColorMap::getBounds(double& low, double &high) const
{
  low = mLow;
  high = mHigh;

  return 1;
}

int VisusColorMap::setFloorColor(float r, float g, float b, float a)
{
  mMap[0] = r;
  mMap[1] = g;
  mMap[2] = b;
  mMap[3] = a;

  incVersion();
  return 1;
}

int VisusColorMap::setCeilingColor(float r, float g, float b, float a)
{
  mMap[4*(mResolution+1)] = r;
  mMap[4*(mResolution+1)+1] = g;
  mMap[4*(mResolution+1)+2] = b;
  mMap[4*(mResolution+1)+3] = a;

  incVersion();
  return 1;
}

GLuint VisusColorMap::texId()
{
  if (mTexId == 0) {
    glGenTextures(1,&mTexId);
   
    glBindTexture(GL_TEXTURE_1D, mTexId);

    glTexParameterf(GL_TEXTURE_1D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
    glTexParameterf(GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

    glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,mResolution,
                 0,GL_RGBA,GL_FLOAT,mMap+4);
    
  }

  return mTexId;
}

int VisusColorMap::style(VisusColorMapStyle style, double low, double high)
{
  if (low == high) {
    low  = 0;
    high = 1;
    mInitialized = false;
  }
  else
    mInitialized = true;


  if (fabs(high - low) < 10e-8) {
    vwarning("Possible division by 0. Function range to small for color map.");
    return 0;
  }

  incVersion();

  mLow = low;
  mHigh = high;

  mStyle = style;

  switch (style) {

  case VISUS_LINEAR_MAP:
    
    // The mapping should be done via [(f - low) / (high -
    // low)]*mResolution + 0.5 + 1. (The 0.5 is the rounding , the "1"
    // adjusts for the anchor color
    
    mMapScale = (mResolution - 1) / (high - low);
    mMapAnchor = low - 1.5/mMapScale;

    mMapFunction = &VisusColorMap::linearMapping;
    break;

  case VISUS_LOG_MAP:

    // The mapping will be done such that (f-x)*y = 1 for f=low and
    // (f-x)*y = e for f=high. Furthermore, we have
    // ln((f-x)*y)*mResolution + 1.5
    
    mMapScale = (M_E - 1) / (high - low);
    mMapAnchor = low - 1/mMapScale;

    mMapFunction = &VisusColorMap::logarithmicMapping;
    break;

  case VISUS_EXP_MAP:

    // The mapping will be done as (exp((f-x)*y)-1)*mResolution + 1.5
    // such that (f-x)*y = 0 for f=low and (f-x)*y = ln2 for f=high.

    mMapScale = log(2.0) / (high - low);
    mMapAnchor = low;

    mMapFunction = &VisusColorMap::exponentialMapping;
    break;
  }

  return 1;
}


void VisusColorMap::toXML(XMLNode& parent) const
{
  XMLNode cm = parent.addChild(XML_TAG);
  cm.addAttribute("type", mType);
  cm.addAttribute("resolution", mResolution);
  if (mType == VISUS_CUSTOM_MAP) 
  {
    cm.addAttribute("storage", VisusXMLInterface::sWriteXMLDataStorage);
    switch (VisusXMLInterface::sWriteXMLDataStorage)
    {
      case ASCII:
      {
        // Save Map In Ascii
        std::stringstream ss;
        for (int i=0; i<4*mResolution; ++i) {
          ss << mMap[i] << " ";
        }
        cm.addText(ss.str().c_str());
        break;
      }
      case BASE64:
      {
        XMLParserBase64Tool base64;
        XMLSTR encoded = base64.encode(reinterpret_cast<unsigned char*>(mMap), sizeof(float)*4*mResolution);
        cm.addText(encoded);
        break;
      }
      default:
        vwarning("VisusXMLInterface storage type is not supported for VisusColorMap");
        vverbose("VisusXMLInterface storage type is not supported for VisusColorMap", VISUS_XML_VERBOSE);
        break;
    }
  }
}

bool VisusColorMap::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) 
  {
    std::stringstream ss;
    ss << "VisusColorMap did not receive top level node.  received (" << node.getName() << ")\n";
    vwarning(ss.str().c_str());
    vverbose(ss.str().c_str(), VISUS_XML_VERBOSE);
    return false;
  }

  mType = (VisusColorMapType) xmltoi(node.getAttribute("type"), mType);
  mResolution = xmltoi(node.getAttribute("resolution"), mResolution);

  if (mType == VISUS_CUSTOM_MAP) 
  {
    XMLDataStorage storage = (XMLDataStorage)
      xmltoi(node.getAttribute("storage"), VisusXMLInterface::sWriteXMLDataStorage);
    vverbose("Load VisusColorMap from storage (%d)\n", VISUS_XML_VERBOSE, storage);

    if (mMap!=NULL) {
      delete [] mMap;
      mMap = new float[4*mResolution];
    }

    switch (storage)
    {
      case ASCII:
      {
        // Get Map From
        std::stringstream ss;
        ss << node.getText();

        for (int i=0; i<4*mResolution; ++i) {
          ss >> mMap[i];
        }
        break;
      }
      case BASE64:
      {
        XMLParserBase64Tool base64;
        base64.decode(node.getText(), reinterpret_cast<unsigned char*>(mMap), 4*mResolution*sizeof(float));
        break;
      }
      default:
        vwarning("VisusXMLInterface storage type is not supported for VisusColorMap");
        vverbose("VisusXMLInterface storage type is not supported for VisusColorMap", VISUS_XML_VERBOSE);
        return false;
    }
  }
  else {
    // We need to load appropriate color map
    (*this) = construct_color_map(mType);
  }
  return true;
}  


