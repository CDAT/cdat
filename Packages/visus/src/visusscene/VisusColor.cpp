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


#if WIN32
#include <windows.h>
#endif

#include "glew.h"
#include "xmlParser.h"
#include <string.h>

#include "VisusColor.h"
#include "VisusAssert.h"

const char* VisusColor::XML_TAG = "VisusColor";

VisusColor::VisusColor()
{
  mColor[0] = 1.0;
  mColor[1] = 1.0;
  mColor[2] = 1.0;
  mColor[3] = 1.0;
}

VisusColor::VisusColor(float r, float g, float b, float a)
{
  mColor[0] = r;
  mColor[1] = g;
  mColor[2] = b;
  mColor[3] = a;
}

VisusColor::VisusColor(const VisusColor& color)
{
  *this = color;
}

VisusColor& VisusColor::operator=(const VisusColor& color)
{
  mColor[0] = color.mColor[0];
  mColor[1] = color.mColor[1];
  mColor[2] = color.mColor[2];
  mColor[3] = color.mColor[3];

  return *this;
}


void VisusColor::glColor() const
{
  glColor4fv(mColor);
}

void VisusColor::toXML(XMLNode& parent) const
{
  XMLNode color = parent.addChild(XML_TAG);
  color.addAttribute("r", mColor[0]);
  color.addAttribute("g", mColor[1]);
  color.addAttribute("b", mColor[2]);
  color.addAttribute("a", mColor[3]);
}

bool VisusColor::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) 
  {
    std::stringstream ss;
    ss << "VisusColor did not receive top level node. received (" << node.getName() << ")\n";
    vwarning(ss.str().c_str());
    vverbose(ss.str().c_str(), VISUS_XML_VERBOSE);
    return false;
  }

  mColor[0] = xmltoi(node.getAttribute("r"), 255);
  mColor[1] = xmltoi(node.getAttribute("g"), 255);
  mColor[2] = xmltoi(node.getAttribute("b"), 255);
  mColor[3] = xmltoi(node.getAttribute("a"), 255);

  return true;
}
