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

#include "VisusSphereNode.h"

VisusSphereNode::VisusSphereNode() : VisusGroup(), VisusSphere()
{
    mNodeType = VISUS_SPHERE_NODE;
}

void VisusSphereNode::display3D(VisusTransformation3D model_view_3D)
{
    VisusTransformation3D local;
    getValue(local);
    
    model_view_3D *= local;
    
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadMatrixf(model_view_3D);
    
    if (mDrawBoundingBox)
	displayBoundingBox();
    
    render();
    
    
    recurse(model_view_3D);

    glPopMatrix();
}

void VisusSphereNode::toXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName()))
    vwarning("failed to receive top level node");

  node.addAttribute("radius", _radius);
  node.addAttribute("originx", _origin[0]);
  node.addAttribute("originy", _origin[1]);
  node.addAttribute("originz", _origin[2]);
  node.addAttribute("numTheta", _numtheta);
  node.addAttribute("numPhi", _numphi);
  node.addAttribute("texEnabled", _texEnabled);
  
  if (_texEnabled) {
    node.addAttribute("format", (long)_format);
    node.addAttribute("width", _wid);
    node.addAttribute("height", _hgt);
    XMLNode child = node.addChild("pixels");

    int bufsize = sizeof(_pixels);
    child.addAttribute("size", bufsize);

    XMLParserBase64Tool base64;
    XMLSTR encoded = base64.encode(_pixels, bufsize);
    child.addText(encoded);
  }
}

bool VisusSphereNode::fromXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("failed to receive top level node");
    return false;
  }

  _radius   = xmltof(node.getAttribute("radius"), _radius);
  _origin[0]= xmltof(node.getAttribute("originx"), _origin[0]);
  _origin[1]= xmltof(node.getAttribute("originy"), _origin[1]);
  _origin[2]= xmltof(node.getAttribute("originz"), _origin[2]);
  _numtheta = xmltoi(node.getAttribute("numTheta"), _numtheta);
  _numphi = xmltoi(node.getAttribute("numPhi"), _numphi);
  _texEnabled = xmltobool(node.getAttribute("texEnabled"), false);
  
  if (_texEnabled) {
    _format = xmltoi(node.getAttribute("format"));
    _wid = xmltoi(node.getAttribute("width"));
    _hgt = xmltoi(node.getAttribute("height"));

    XMLNode child = node.getChildNode("pixels");
    int bufsize = xmltoi(child.getAttribute("size"));

    if (_pixels)
      delete [] _pixels;
    _pixels = new unsigned char[bufsize+1];

    XMLParserBase64Tool base64;
    base64.decode(child.getText(), _pixels, bufsize);
  }

  return true;
}
