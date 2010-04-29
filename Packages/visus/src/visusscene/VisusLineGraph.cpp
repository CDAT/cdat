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

#include "VisusBlockData.h"
#include "VisusLineGraph.h"

#include "glew.h"
#include <math.h>


pVisusLineGraph VisusLineGraph::instantiate()
{
  return gObjectFactory.instantiate<VisusLineGraph>();
}


VisusLineGraph::VisusLineGraph():VisusGraph(VISUS_LINE_GRAPH), VisusConsumer(0)
{  
  mPointSize = 1.0;
  mLineWidth = 1.0;

  mAutoAdjustX = true;
  mAutoAdjustY = true;
}  

VisusLineGraph::~VisusLineGraph()
{
  for (unsigned int i=0; i<mData.size(); ++i) {
    delete mData[i];
  }
}

void VisusLineGraph::autoAdjustMinMax(const BBAxis axis, bool autoAdjustOn) 
{ 
  switch (axis)
  {
  case BB_X_AXIS:
    mAutoAdjustX = autoAdjustOn; 
    break;
  case BB_Y_AXIS:
    mAutoAdjustY = autoAdjustOn; 
    break;
  }
}


bool VisusLineGraph::numberOfLines(const int numLines)
{
  verror(!setNumberOfInputs(numLines), "failed to correctly set number of inputs", false);

  float r=1.0;
  float g=1.0;
  float b=1.0;
  float a=1.0;

  float delta = 1.0 / (mNrOfInputs+7);

  for (int i=0; i<mNrOfInputs; ++i) {
    mLineColor.push_back(VisusColor(r,g,b,a));
    r -= delta;
    g -= delta;
    b -= delta;
  }

  mData.resize(numLines);
  for (int i=0; i<numLines; ++i) {
    mData[i] = new VisusBlockData();
    mSinks[i]= mData[i];
  }

  return true;
}

bool VisusLineGraph::lineColor(const int i, const VisusColor& color)
{
  verror(i >= mNrOfInputs || i < 0, "given index out of range", false);

  mLineColor[i] = color;

  return true;
}

bool VisusLineGraph::pointSize(const float size)
{
  mPointSize = size;
  return true;
}

bool VisusLineGraph::lineWidth(const float width)
{
  mLineWidth = width;
  return true;
}


void VisusLineGraph::display2D(VisusTransformation2D model_view_2D)
{
  synchronize();

  // Draw Everything In 2D
  render2D(model_view_2D);


  recurse(model_view_2D);
}

bool VisusLineGraph::renderInsideBox(const float boxWidth, const float boxLength)
{
  // Recompute Bounds If Requested
  if (mAutoAdjustX || mAutoAdjustY)
  {
    GraphHelper::MinMaxFunctor minMax(mAutoAdjustX, mXAxis, mAutoAdjustY, mYAxis);

    for (unsigned int i=0; i<mData.size(); ++i)
    {
      minMax.call(*mData[i]);
    }
  }

  // Loop Over And Draw Lines
  for (unsigned int i=0; i<mData.size(); ++i)
  {
    renderLine(boxWidth, boxLength, mLineColor[i], *mData[i]);
  }
  return true;
}


bool VisusLineGraph::renderLine(const float boxWidth, const float boxLength,
                                const VisusColor& color, const VisusBlockData& data)
{
  color.glColor();

  DrawFunctor draw(boxWidth, boxLength, mXAxis, mYAxis);

  // Draw Visible Line Segment
  glLineWidth(mLineWidth);
  glBegin(GL_LINE_STRIP);
  draw.call(data);
  glEnd();

  // Draw Visible Points
  glPointSize(mPointSize);
  glEnable(GL_POINT_SMOOTH);
  glBegin(GL_POINTS);
  draw.call(data);
  glEnd();

  vglerror();
  return true;
}

//===================================================
//  DrawFunctor
//===================================================
VisusLineGraph::DrawFunctor::DrawFunctor(const float boxWidth, const float boxLength,
                                         const VisusBorderAxis& x, const VisusBorderAxis& y)
{
  xMin = x.minValue();
  xMax = x.maxValue();
  yMin = y.minValue();
  yMax = y.maxValue();

  xfactor = boxLength / (xMax - xMin);
  yfactor = boxWidth / (yMax - yMin);
}

void VisusLineGraph::DrawFunctor::process(float x, float y)
{
  if (x < xMin || x > xMax || y < yMin || y > yMax)
    return;
  glVertex2d((x - xMin)*xfactor, (y - yMin)*yfactor);
}

void VisusLineGraph::toXMLLocalVariables(XMLNode& node)
{
  VisusGraph::toXMLLocalVariables(node);

  node.addAttribute("autoAdjustX", mAutoAdjustX);
  node.addAttribute("autoAdjustY", mAutoAdjustY);
  node.addAttribute("pointSize", mPointSize);
  node.addAttribute("lineWidth", mLineWidth);
  node.addAttribute("numLines", (long)mLineColor.size());

  for (unsigned int i=0; i<mLineColor.size(); ++i)
  {
    XMLNode child = node.addChild("line");

    mLineColor[i].toXML(child);

    child.addAttribute("connected", isConnected(i));
    if (isConnected(i)) {
      // Connected track producer
      child.addAttribute("dataProducer", mInputNodes[i]->id());
    }
    else {
      // Not connected, save data
      mData[i]->toXML(child);
    }
  }
}

bool VisusLineGraph::fromXMLLocalVariables(XMLNode& node)
{
  if (! VisusGraph::fromXMLLocalVariables(node)) {
    vwarning("failed to initialize parent class when loading xml");
    return false;
  }

  mAutoAdjustX = xmltobool(node.getAttribute("autoAdjustX"), mAutoAdjustX);
  mAutoAdjustY = xmltobool(node.getAttribute("autoAdjustY"), mAutoAdjustY);
  mPointSize = xmltof(node.getAttribute("pointSize"), mPointSize);
  mLineWidth = xmltof(node.getAttribute("lineWidth"), mLineWidth);

  int numLines = xmltoi(node.getAttribute("numLines"), 0);
  vverbose("VisusLineGraph initializing for %d lines\n", VISUS_XML_VERBOSE, numLines);
  numberOfLines(numLines);

  for (int i=0; i<numLines; ++i)
  {
    XMLNode child = node.getChildNode("line", i);

    XMLNode color = child.getChildNode(VisusColor::XML_TAG);
    mLineColor[i].fromXML(color);

    bool connected = xmltobool(child.getAttribute("connected"), false);
    if (connected) {
      // Connect as consumer
      if (! visusXML::connectConsumer(child, "dataProducer", this, i, mData[i])) {
        vwarning("failed to connect as consumer when loading xml");
        return false;
      }
    }
    else {
      // Load data
      XMLNode data = child.getChildNode(VisusData::XML_TAG);
      if (! mData[i]->fromXML(data)) {
        vwarning("failed to load mesh data from xml");
        return false;
      }
    }
  }
  return true;
}
