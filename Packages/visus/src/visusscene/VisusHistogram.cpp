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
#include "VisusHistogram.h"
#include "VisusMath.h"
#include "glew.h"

#include <iostream>

//======================================================================
//! Functor which draws the points in the histogram
//======================================================================
class DrawFunctor : public GraphHelper::DataFunctor
{
public:
  DrawFunctor(const float boxWidth, const float boxLength,
              const VisusBorderAxis& x, const VisusBorderAxis& y, 
              const float barWidth, const int numBins)
  {
    length = boxLength;
    width = boxWidth;

    halfWidth = ( barWidth * (length/(numBins+1)) ) / 2.0;

    xMin = x.minValue();
    xMax = x.maxValue();
    yMin = y.minValue();
    yMax = y.maxValue();

    xfactor = boxLength / (xMax - xMin);
    yfactor = boxWidth / (yMax - yMin);    
  }
protected:
  void process(float x, float y)
  {
    double v[2];

    if (x < xMin || x > xMax)
      return;
    if (y > yMax) y = yMax;
    else if (y < yMin) y = yMin;

    double minX = x*xfactor - halfWidth;
    double maxX = x*xfactor + halfWidth;
    if (maxX > length) maxX = length;

    double minY = 0;
    double maxY = (y - yMin) * yfactor;

    v[0] = minX;
    v[1] = maxY;
    glVertex2dv(v);

    v[1] = minY;
    glVertex2dv(v);

    v[0] = maxX;
    glVertex2dv(v);

    v[1] = maxY;
    glVertex2dv(v);
  }
  double xMin, xMax, yMin, yMax, yfactor, xfactor, halfWidth, length, width;
};

//======================================================================
//! Functor to draw points in histogram where labels are predefined
//======================================================================
class DrawFixedLabelsFunctor : public DrawFunctor
{
public:
  DrawFixedLabelsFunctor(const float boxWidth, const float boxLength,
                         const VisusBorderAxis& x, const VisusBorderAxis& y, 
                         const float barWidth, const int numBins):
    DrawFunctor(boxWidth, boxLength, x, y, barWidth, numBins)
  {
    currentBin = 1;
    xPerBin = (xMax - xMin) / (numBins+1);
  }
protected:
  void process(float x, float y)
  {
    float v[2];
    // our true x is the even distribution for our bin
    float effectiveX = xPerBin * currentBin;
    DrawFunctor::process(effectiveX, y);
    ++currentBin;
  }
  int currentBin;
  float xPerBin;
};

//======================================================================
//! Functor which draws the points in the histogram evenly distributed
//======================================================================
class DrawEvenDistFunctor : public DrawFunctor
{
public:
  DrawEvenDistFunctor(const float boxWidth, const float boxLength,
                      const VisusBorderAxis& x, const VisusBorderAxis& y, 
                      const float barWidth, const int numBins):
    DrawFunctor(boxWidth, boxLength, x, y, barWidth, numBins)
  {
    xPerBar = boxLength / (numBins+1);   
    xCurrent = xPerBar * 1.5;
  }
protected:
  void process(float x, float y)
  {
    if (x < xMin || x > xMax)
      return;
    if (y > yMax) y = yMax;
    else if (y < yMin) y = yMin;

    double minY = 0;
    double maxY = (y - yMin) * yfactor;

    glVertex2d(xCurrent, minY);
    glVertex2d(xCurrent, maxY);

    xCurrent += xPerBar;
  }
  double xCurrent, xPerBar;
};


pVisusHistogram VisusHistogram::instantiate()
{
  return gObjectFactory.instantiate<VisusHistogram>();
}


VisusHistogram::VisusHistogram():VisusGraph(VISUS_HISTOGRAM), VisusConsumer(1)
{
  mBarColor = VisusColor();
  mBarWidth = 0.5;
  mBarFill = true;
  mAutoAdjust = false;
  mEvenDist = false;
  mEvenDistNumPixelsPerBar = 0;
  mEvenDistNumPixelsForBox = 0;

  mSinks[0] = &mData;
}  

VisusHistogram::~VisusHistogram()
{}


bool VisusHistogram::barColor(const VisusColor& color)
{
  mBarColor = color;
  return true;
}

bool VisusHistogram::barFill(const bool fill)
{
  mBarFill = fill;
  return true;
}

bool VisusHistogram::barWidth(const float width)
{
  verror(width < 0 || width > 1,"invalid value for histogram bar width. must be between 0 and 1",false);
  mBarWidth = width;
  return true;
}

void VisusHistogram::display2D(VisusTransformation2D model_view_2D)
{
  const float xunits=xUnits();
  const float yunits=yUnits();

  synchronize();

  // Adjust X Tick Marks - No Minors and Majors To Match Number of Bins
  std::vector<int> dims = mData.samples();

  if (mXAxis.hasTickLabels()) {
    mXAxis.minorTicks(0);
    mXAxis.majorTicks(dims[0]+2);
  }

  // Possibly update for even distribution among bars
  processEvenDistribution(model_view_2D, dims[0]);

  // Draw Everything In 2D
  render2D(model_view_2D);

  // Reset to user settings
  xUnits(xunits);
  xUnits(yunits);

  recurse(model_view_2D);
}

void VisusHistogram::processEvenDistribution(VisusTransformation2D model_view_2D, const int numHistogramBars)
{
  if (mEvenDist)
  {
    const int numBars = numHistogramBars + 1; // consistent with draw

    // Position Appropriately
    VisusTransformation2D local;
    getValue(local);
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
    model_view_2D *= local;
    glTranslatef(model_view_2D[6],model_view_2D[7],0);

    // Get Current Settings
    GLint viewport[4];
    GLdouble projection[16], model[16];
    glGetIntegerv(GL_VIEWPORT, viewport);
    glGetDoublev(GL_MODELVIEW_MATRIX, model);
    glGetDoublev(GL_PROJECTION_MATRIX, projection);
    int canvasPixels = viewport[2];
    if (mOrientation == BB_VERTICAL)
      canvasPixels = viewport[3];

    // Ensure that width of box is evenly divisible by number of items on axis
    // so that spacing between items is consistent 
    {
      // Determine number of pixels contained in the graph "box" for X axis
      float length = unitLength() * xUnits();
      double numBoxPixels = numPixels(model,projection,viewport,length,0);

      mEvenDistNumPixelsPerBar = std::max((long)1, (long)round(numBoxPixels / numBars));
      mEvenDistNumPixelsForBox = mEvenDistNumPixelsPerBar * numBars;
      const double changeInNumPixels = (1.0*mEvenDistNumPixelsForBox) / numBoxPixels;
      float xunit = xUnits() * changeInNumPixels;
      xUnits(xunit);
    }  

    glPopMatrix();
  }
}

bool VisusHistogram::renderInsideBox(const float boxWidth, const float boxLength)
{
  // Gather Statistics about data
  if (mAutoAdjust) {
    GraphHelper::MinMaxFunctor minMax(mAutoAdjust,mXAxis,mAutoAdjust,mYAxis);
    minMax.call(mData);
  }

  // One final readjust to assure bars look pretty
  GraphHelper::DistanceFunctor distance(mXAxis.minValue(),mXAxis.maxValue());
  distance.call(mData);
  if (mAutoAdjust) {
    float stdDist = distance.avgDist();
    mXAxis.minValue(mXAxis.minValue() - stdDist);
    mXAxis.maxValue(mXAxis.maxValue() + stdDist);
    if (mYAxis.minValue() < 75.0) mYAxis.minValue(0.0);
  }

  // Set Color
  mBarColor.glColor();

  std::vector<int> dims = mData.samples();

  if (!mXAxis.hasTickLabels() && mEvenDist) {
    glLineWidth(mBarWidth * mEvenDistNumPixelsPerBar);
    glBegin(GL_LINES);

    DrawEvenDistFunctor draw(boxWidth,boxLength,mXAxis,mYAxis,mBarWidth,dims[0]);
    draw.call(mData);

    glEnd();
    glLineWidth(1.0);
  }
  else {
    // Set Up to Fill Or Hollow
    if (! mBarFill)
      glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
    else
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

    // Draw The Bars
    glBegin(GL_QUADS);

    if (mXAxis.hasTickLabels()) {
      DrawFixedLabelsFunctor draw(boxWidth,boxLength,mXAxis,mYAxis,mBarWidth,dims[0]);
      draw.call(mData);
    }
    else {
      DrawFunctor draw(boxWidth,boxLength,mXAxis,mYAxis,mBarWidth,dims[0]);
      draw.call(mData);
    }

    glEnd();

    // Reset Poly Mode
    glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  }

  return true;
}

void VisusHistogram::toXMLLocalVariables(XMLNode& node)
{
  VisusGraph::toXMLLocalVariables(node);

  node.addAttribute("autoAdjust", mAutoAdjust);
  node.addAttribute("evenDist", mEvenDist);
  node.addAttribute("barWidth", mBarWidth);
  node.addAttribute("barFill", mBarFill);

  XMLNode child = node.addChild("barColor");
  mBarColor.toXML(child);

  node.addAttribute("connected", isConnected(0));
  if (isConnected(0)) {
    // Connected track producer
    node.addAttribute("dataProducer", mInputNodes[0]->id());
  }
  else {
    // Not connected, save data
    mData.toXML(node);
  }
}

bool VisusHistogram::fromXMLLocalVariables(XMLNode& node)
{
  if (! VisusGraph::fromXMLLocalVariables(node)) {
    vwarning("failed to initialize parent class when loading xml");
    return false;
  }

  mAutoAdjust = xmltobool(node.getAttribute("autoAdjust"), mAutoAdjust);
  mEvenDist = xmltobool(node.getAttribute("evenDist"), mEvenDist);
  mBarWidth = xmltof(node.getAttribute("barWidth"), mBarWidth);
  mBarFill = xmltobool(node.getAttribute("barFill"), mBarFill);

  XMLNode child = node.getChildNode("barColor");
  child = child.getChildNode(0);
  mBarColor.fromXML(child);

  bool connected = xmltobool(node.getAttribute("connected"), false);

  if (connected) {
    // Connect as consumer
    if (! visusXML::connectConsumer(node, "dataProducer", this, 0, &mData)) {
      vwarning("failed to connect as consumer when loading xml");
      return false;
    }
  }
  else {
    // Load mesh data
    XMLNode data = node.getChildNode(VisusData::XML_TAG);
    if (! mData.fromXML(data)) {
      vwarning("failed to load mesh data from xml");
      return false;
    }
  }

  return true;
}
