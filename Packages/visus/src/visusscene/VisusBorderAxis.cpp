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

#include "VisusBorderAxis.h"
#include "VisusAssert.h"
#include <iostream>

 /***************************************************************
  ******       Default Values                           *********
  **************************************************************/  
const char*         VisusBorderAxis::XML_TAG = "VisusBorderAxis";
const char          VisusBorderAxis::DEFAULT_FORMAT_STRING[10] = "%0.1f";
const int           VisusBorderAxis::DEFAULT_LABEL_SIZE        = 8;  
const AXISSide      VisusBorderAxis::DEFAULT_LABEL_POSITION    = AXIS_LOW;
const AXISAlignment VisusBorderAxis::DEFAULT_LABEL_ALIGNMENT   = AXIS_CENTER_ALIGN;
const float         VisusBorderAxis::DEFAULT_LABEL_OFFSET      = 0.01;
const int           VisusBorderAxis::DEFAULT_LEGEND_SIZE       = 12;
const AXISSide      VisusBorderAxis::DEFAULT_LEGEND_POSITION   = AXIS_LOW;
const AXISAlignment VisusBorderAxis::DEFAULT_LEGEND_ALIGNMENT  = AXIS_CENTER_ALIGN;
const float         VisusBorderAxis::DEFAULT_LEGEND_OFFSET     = 0.03;
const float         VisusBorderAxis::DEFAULT_MAJOR_LENGTH      = 1.0;
const float         VisusBorderAxis::DEFAULT_MAJOR_THICKNESS   = 0.002;
const float         VisusBorderAxis::DEFAULT_MINOR_LENGTH      = 0.5;
const float         VisusBorderAxis::DEFAULT_MINOR_THICKNESS   = 0.001;
const AXISSide      VisusBorderAxis::DEFAULT_TICK_POSITION     = AXIS_LOW;
const float         VisusBorderAxis::DEFAULT_MIN_VALUE         = 0;
const float         VisusBorderAxis::DEFAULT_MAX_VALUE         = 1;
const int           VisusBorderAxis::DEFAULT_MAJOR_TICKS       = 3;
const int           VisusBorderAxis::DEFAULT_MINOR_TICKS       = 4;

//----------------------------------------------------------------------------
// Constructor sets a default format for the tick labels, and a default title
//----------------------------------------------------------------------------
VisusBorderAxis::VisusBorderAxis():
  mLabel(DEFAULT_FORMAT_STRING), mLegend("Legend")
{
  // Enable drawing of ticks, label and legend
  enable();

  // Create default tick labels characteristics
  mLabelPosition = DEFAULT_LABEL_POSITION;
  mLabelAlignment = DEFAULT_LABEL_ALIGNMENT;
  mLabelOffset = DEFAULT_LABEL_OFFSET;
  VisusFont labelFont;
  labelFont.fontSize(DEFAULT_LABEL_SIZE);
  mLabel.font(labelFont);
  
  // Create default legend characteristics
  mLegendPosition = DEFAULT_LEGEND_POSITION;
  mLegendAlignment = DEFAULT_LEGEND_ALIGNMENT;
  mLegendOffset = DEFAULT_LEGEND_OFFSET;
  VisusFont legendFont;
  legendFont.fontSize(DEFAULT_LEGEND_SIZE);
  mLegend.font(legendFont);

  // Set the default characteristics for the ticks marks
  mMajorTickLength = DEFAULT_MAJOR_LENGTH;
  mMajorTickThickness = DEFAULT_MAJOR_THICKNESS;
  mMinorTickLength = DEFAULT_MINOR_LENGTH;
  mMinorTickThickness = DEFAULT_MINOR_THICKNESS;
  mTickPosition = DEFAULT_TICK_POSITION;
  mMinValue = DEFAULT_MIN_VALUE;
  mMaxValue = DEFAULT_MAX_VALUE;
  mMajorTicks = DEFAULT_MAJOR_TICKS;
  mMinorTicks = DEFAULT_MINOR_TICKS;    

  mDrawAxis = 0;
  mDrawOther = 1;
}  

//----------------------------------------------------------------------------
// Copy constructor
//----------------------------------------------------------------------------
VisusBorderAxis::VisusBorderAxis(const VisusBorderAxis& config)
{
  // Invoke assignment operator
  *this = config;
}

//----------------------------------------------------------------------------
// Destructor
//----------------------------------------------------------------------------
VisusBorderAxis::~VisusBorderAxis()
{
}

//----------------------------------------------------------------------------
// Assignment operator
//----------------------------------------------------------------------------
VisusBorderAxis& VisusBorderAxis::operator=(const VisusBorderAxis& config)
{
  // Assign the labels
  mDrawLabels = config.mDrawLabels;
  mLabelPosition = config.mLabelPosition;
  mLabelAlignment = config.mLabelAlignment;
  mLabelOffset = config.mLabelOffset;
  mLabel.text(config.mLabel.text());
  mLabel.font(config.mLabel.font());
  mLabel.orientation(config.mLabel.orientation());

  // Assign the legend
  mDrawLegend = config.mDrawLegend;
  mLegendPosition = config.mLegendPosition;
  mLegendAlignment = config.mLegendAlignment;
  mLegendOffset = config.mLegendOffset;
  mLegend.text(config.mLegend.text());
  mLegend.font(config.mLegend.font());
  mLegend.orientation(config.mLegend.orientation());

  // Assign the ticks
  mDrawTicks = config.mDrawTicks;
  mMajorTickLength = config.mMajorTickLength;
  mMajorTickThickness = config.mMajorTickThickness;
  mMinorTickLength = config.mMinorTickLength;
  mMinorTickThickness = config.mMinorTickThickness;
  mTickPosition = config.mTickPosition;
  mMinValue = config.mMinValue;
  mMaxValue = config.mMaxValue;
  mMajorTicks = config.mMajorTicks;
  mMinorTicks = config.mMinorTicks;
  mTickColor = config.mTickColor;

  // Grab the usr defined tick labels, if any
  mTickLabels = config.mTickLabels;

  mDrawBox = config.mDrawBox;
  mDrawAxis = config.mDrawAxis;
  mDrawOther = config.mDrawOther;

  return *this;
}

//----------------------------------------------------------------------------
// Enable drawing of ticks, legend and labels
//----------------------------------------------------------------------------
void VisusBorderAxis::enable()
{
  mDrawTicks = true;
  mDrawLabels = true;
  mDrawLegend = true;
}

//----------------------------------------------------------------------------
// Disnable drawing of ticks, legend and labels
//----------------------------------------------------------------------------
void VisusBorderAxis::disable()
{
  mDrawTicks = false;
  mDrawLabels = false;
  mDrawLegend = false;
}

//----------------------------------------------------------------------------
// Set the text for the tick labels
//----------------------------------------------------------------------------
void VisusBorderAxis::labelText(const char* text)
{
  mLabel.text(text);
}

//----------------------------------------------------------------------------
// Set the font for the label
//----------------------------------------------------------------------------
void VisusBorderAxis::labelFont(const VisusFont& font)
{
  mLabel.font(font);
}

//----------------------------------------------------------------------------
// Set the text of the legend (title) of the colorbar
//----------------------------------------------------------------------------
void VisusBorderAxis::legendText(const char* text)
{
  mLegend.text(text);
}

//----------------------------------------------------------------------------
// Set the font of the legend (title)
//----------------------------------------------------------------------------
void VisusBorderAxis::legendFont(const VisusFont& font)
{
  mLegend.font(font);
}

//----------------------------------------------------------------------------
// Set the color of the tick marks
//----------------------------------------------------------------------------
void VisusBorderAxis::tickColor(const VisusColor& color)
{
  mTickColor = color;
}

//----------------------------------------------------------------------------
// Set a different string for every tick mark
//----------------------------------------------------------------------------
void VisusBorderAxis::tickLabels(int numLabels, std::string tickLabels[])
{
  mTickLabels.clear();
  for (int i=0; i<numLabels; ++i) {
    mTickLabels.push_back(tickLabels[i]);
  }
  majorTicks(numLabels);
}

void VisusBorderAxis::tickLabels(const std::vector<std::string>& tickLabels)
{
  mTickLabels.clear();
  if (tickLabels.size() > 0) {
    mTickLabels = tickLabels;
    majorTicks(mTickLabels.size());
  }
}

//---------------------------------------------------------------------------
// Draw the major and minor tick marks
//----------------------------------------------------------------------------
void VisusBorderAxis::renderTickMarks(const float borderWidth, const float borderLength,
                                      const BBAxis axis, const BBTickLine tickLineType)
{ 
  // If drawing ticks is disabled, return
  if(!mDrawTicks)
    return;
  
  float low,high,minor_low,minor_high;
  float dimensions[2] = { borderLength, borderWidth };
  int other = (axis+1) % 2;
  float width = dimensions[other];
  float length = dimensions[axis];

  // Get the current viewport
  GLdouble model[16];
  GLdouble projection[16];
  GLint viewport[4];  // x, y, width, height
  glGetDoublev(GL_PROJECTION_MATRIX, projection);
  glGetDoublev(GL_MODELVIEW_MATRIX, model);
  glGetIntegerv(GL_VIEWPORT,viewport); 

  // Determine number of pixels per tick mark based on thickness (smallest 1)
  float minval = numPixels(model, projection, viewport, length, axis);
  const int numPixelPerMajorTick = std::max(1, (int)floor(minval*mMajorTickThickness));
  const int numPixelPerMinorTick = std::max(1, (int)floor(minval*mMinorTickThickness));
    
  float majorDelta = length / (mMajorTicks-1);
  float minorDelta = majorDelta / mMinorTicks;

  // Determine Tick Up/Down Positions
  low = minor_low = 0;
  high = minor_high = width;
  
  // If we need to draw tick on the normal side (below or right)
  if ((mTickPosition == AXIS_LOW) || (mTickPosition == AXIS_BOTH)) 
    {
      low -= mMajorTickLength * width; // it needs to start this much below 0
      minor_low -= mMinorTickLength * mMajorTickLength * width;
    }

  // If we need to draw tick on the opposite side (above or left)
  if ((mTickPosition == AXIS_HIGH) || (mTickPosition == AXIS_BOTH)) 
    {
      high += mMajorTickLength * width; // it needs to end this much above the bar
      minor_high += mMinorTickLength * mMajorTickLength * width;
    }

  // Set the bounding box
  if (axis == BB_X_AXIS) {
    mDrawBox[0] = 0; 
    mDrawBox[1] = low;
    mDrawBox[3] = length; 
    mDrawBox[4] = high;
  }
  else {
    mDrawBox[0] = low; 
    mDrawBox[1] = 0;
    mDrawBox[3] = high; 
    mDrawBox[4] = length;	  
  }

  mWidth = width;
  mLength= length;
  mDrawAxis = axis;
  mDrawOther = other;
  
  if (! mDrawTicks)
    return;

  mTickColor.glColor();
      
  // We can't change the thickness between a glBegin() and
  // glEnd(). Therefore, we first draw the major ticks then the minor
  // ones
  glLineWidth(numPixelPerMajorTick);
  glBegin(GL_LINES);
  float axisPosition = 0;
  for (int i=0;i<mMajorTicks; ++i, axisPosition+=majorDelta) {
    if (i==0 || i==(mMajorTicks-1))
    	drawTick(low, 0, axisPosition, BB_SOLID);   
    else
    	drawTick(low, high, axisPosition, tickLineType);    
  }
  glEnd();

  
  // Now draw the minor ticks
  glLineWidth(numPixelPerMinorTick);
  glBegin(GL_LINES);
  axisPosition = 0;
  for (int i=0;i<mMajorTicks-1; ++i, axisPosition+=majorDelta) 
    {	  
      for (int j=0;j<mMinorTicks; ++j) 
	{
	  float position = axisPosition + (minorDelta*j);
	  drawTick(minor_low, minor_high, position, tickLineType);
	}
    }
  glEnd();
}

//---------------------------------------------------------------------------
// Draw an individual tick
//---------------------------------------------------------------------------
void VisusBorderAxis::drawTick(const float low, const float high, const float axisPosition, 
                               const BBTickLine tickLineType)
{
  float v[2];

  if (tickLineType==BB_DASHED) {
    float delta = (high-low) / 50;
    float begin = low;
    for (int i=0; i<25; ++i) {
      drawTick(begin, begin+delta, axisPosition, BB_SOLID);
      begin += (delta*2);
    }
    return;
  }

  v[mDrawAxis] = axisPosition;
  v[mDrawOther]= low;
  glVertex2fv(v);

  v[mDrawOther]= high;
  glVertex2fv(v);
}

//---------------------------------------------------------------------------
// Draw the tick labels
//---------------------------------------------------------------------------
void VisusBorderAxis::renderLabels(const int canvasWidth, const int canvasHeight)
{
  //std::cout << "VisusBorderAxis::RenderLabels" << std::endl;
  
  // If drawing is disabled, return
  if(!mDrawLabels)
    return;

  // Labels need to be rendered below(above) the tick marks with a
  // certain offset in between. First we calculate the
  // baseline/topline which gives the translation in y	
  VisusBoundingBox bbox = mLabel.bbox("T");
  
  // Calculate the base-/topline as if no tick marks exist
  // The text below the bar will be rendered at this y-coordinate
  float baseline = mDrawBox[mDrawOther] - (bbox[mDrawOther+3] - bbox[mDrawOther]) - mLabelOffset;
  // The text above the bar will be rendered at this y-coordinate
  float topline  = mDrawBox[mDrawOther+3] + mLabelOffset;

  // Now compute the actual labels 
  float tmp = 0;
  float value;
  char label[50];
  float trans[3] = {0,0,0}; // The translation caused by the label alignment

  float delta = mLength / (mMajorTicks - 1);
  
  int labelIndex=0;
  
  // For all major ticks (minors don't have labels)
  for (int i = 0;i < mMajorTicks;i++) {
    
    // compute the numerical value
    value = mMinValue + i*(mMaxValue - mMinValue) / (mMajorTicks - 1);
    
    // create the actual string to render
    if (mTickLabels.size() > 0) {

      //  REMOVED: this puts extra empty tick labels so that the user-passed in
      // ticks appear in the middle of the chart....
      //if (i==0 || (i+1)==mMajorTicks) 
      //  {
      //   strcpy(label, "");
      // }
      // else 
      // {
      strcpy(label,mTickLabels[labelIndex].c_str());
      ++labelIndex;
      // }
    }
    else  {
      sprintf(label,mLabel.text(),value);
    }
    
    // get the bounding box of the label
    bbox = mLabel.bbox(label);
    
    float bboxDims[6];
    bbox.get(bboxDims);
    
    //std::cout << "bbox : " << std::endl;
    //for(int i = 0; i < 6; i++)
    //	std::cout << bboxDims[i] << " ";
    //std::cout << std::endl;
    //std::cout << "mLableAlignmant: " << mLabelAlignment << std::endl;
    
    // Compute the alignment to the ticks
    switch (mLabelAlignment) {
      
    case AXIS_LEFT_ALIGN:
      trans[mDrawAxis] = 0;
      break;
    case AXIS_RIGHT_ALIGN:
      trans[mDrawAxis] = -(bbox[mDrawAxis+3] - bbox[mDrawAxis]);
      break;
    case AXIS_CENTER_ALIGN:
      trans[mDrawAxis] = -(bbox[mDrawAxis+3] - bbox[mDrawAxis])/2;
      break;
    }     
      
    trans[mDrawAxis] += tmp;
    
    // std::cout << "mLabel oriendt: " << mLabel.orientation() << std::endl;
    //std::cout << "label: " << label << std::endl;
    //std::cout << "trans: " << trans[0] << " " << trans[1] << std::endl;
    
    // If we have to draw bottom labels
    if ((mLabelPosition == AXIS_LOW) ||(mLabelPosition == AXIS_BOTH)) {
      trans[mDrawOther] = baseline;
      if (mDrawOther==0) { 
        trans[mDrawOther] -= (bbox[3] - bbox[0]);
      }
	
      
      glTranslatef(trans[0],trans[1],0);	
      //
      if (mDrawLabels)
        mLabel.render(canvasHeight,label);   
      
      glTranslatef(-trans[0],-trans[1],0);      
      
      mDrawBox += bbox.translate(trans);      
    }  
    
    // If we have to draw top labels
    if ((mLabelPosition == AXIS_HIGH) ||(mLabelPosition == AXIS_BOTH)) {
      trans[mDrawOther] = topline;
      
      glTranslatef(trans[0],trans[1],0);
      if (mDrawLabels)
        mLabel.render(canvasHeight,label);      
      glTranslatef(-trans[0],-trans[1],0);      
      
      mDrawBox += bbox.translate(trans);      
    }
    
    tmp += delta;
  }
  
  vglerror();
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void VisusBorderAxis::renderLegend(const int canvasWidth, const int canvasHeight)
{

  // If drawing the legend is disabled, return
  if(!mDrawLegend)
    return;

  VisusBoundingBox bbox;

  float trans[2] = { 0, 0};
  
  bbox = mLegend.bbox();

  switch (mLegendAlignment) {
  case AXIS_LEFT_ALIGN:
    trans[mDrawAxis]= 0;
    break;
  case AXIS_RIGHT_ALIGN:
	  trans[mDrawAxis]= mDrawBox[mDrawAxis+3] - (bbox[mDrawAxis+3] - bbox[mDrawAxis]);
    break;
  case AXIS_CENTER_ALIGN:
	  trans[mDrawAxis]= (mDrawBox[mDrawAxis+3] + mDrawBox[mDrawAxis])/2 - (bbox[mDrawAxis+3] - bbox[mDrawAxis])/2;
    break;
  }     
  
  
  if ((mLegendPosition == AXIS_LOW) ||(mLegendPosition == AXIS_BOTH)) 
  {
    // Translate to Correct Location
	  trans[mDrawOther] = mDrawBox[mDrawOther]-(bbox[mDrawOther+3] - bbox[mDrawOther]); 
    glTranslatef(trans[0],trans[1],0);
    
    if (mDrawLegend)
      mLegend.render(canvasHeight);
        	
    // Reverse Translation
    glTranslatef(-trans[0],-trans[1],0);
  }

  if ((mLegendPosition == AXIS_HIGH) ||(mLegendPosition == AXIS_BOTH)) 
  { 
    // Translate to Correct Location
	  trans[mDrawOther] = mDrawBox[mDrawOther+3]+mLegendOffset; 
    glTranslatef(trans[0],trans[1],0);
    
    if (mDrawLegend)
      mLegend.render(canvasHeight);

    // Reverse Translation
    glTranslatef(-trans[0],-trans[1],0);
  }
  vglerror();
}
  
//---------------------------------------------------------------------------
// Write to xml
//---------------------------------------------------------------------------
void VisusBorderAxis::toXML(XMLNode& parent) const
{
  XMLNode node = parent.addChild(XML_TAG);

  XMLNode child = node.addChild("label");
  mLabel.toXML(child);
  node.addAttribute("drawLabel", mDrawLabels);
  node.addAttribute("labelPosition", mLabelPosition);
  node.addAttribute("labelAlignment", mLabelAlignment);
  node.addAttribute("labelOffset", mLabelOffset);

  child = node.addChild("legend");
  mLegend.toXML(child);
  node.addAttribute("drawLegend", mDrawLegend);
  node.addAttribute("legendPosition", mLegendPosition);
  node.addAttribute("legendAlignment", mLegendAlignment);
  node.addAttribute("legendOffset", mLegendOffset);

  node.addAttribute("drawTicks", mDrawTicks);
  node.addAttribute("majorTickLength", mMajorTickLength);
  node.addAttribute("majorTickThickness", mMajorTickThickness);
  node.addAttribute("minorTickLength", mMinorTickLength);
  node.addAttribute("minorTickThickness", mMinorTickThickness);
  node.addAttribute("tickPosition", mTickPosition);

  node.addAttribute("min", mMinValue);
  node.addAttribute("max", mMaxValue);
 
  node.addAttribute("majorTicks", mMajorTicks);
  node.addAttribute("minorTicks", mMinorTicks);
 
  XMLNode tickColor = node.addChild("TickColor");
  mTickColor.toXML(tickColor);
}

//---------------------------------------------------------------------------
// Read from xml
//---------------------------------------------------------------------------
bool VisusBorderAxis::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) 
  {
    std::stringstream ss;
    ss << "VisusBorderAxis did not receive its top level node. received (" << node.getName() << ")\n";
    vwarning(ss.str().c_str());
    vverbose(ss.str().c_str(), VISUS_XML_VERBOSE);
    return false;
  }

  XMLNode child = node.getChildNode("label");
  XMLNode text  = child.getChildNode(0);
  if (! mLabel.fromXML(text)) {
    vwarning("Failed to retrieve label text for VisusBorderAxis");
    return false;
  }

  mDrawLabels = xmltobool(node.getAttribute("drawLabel"), mDrawLabels);
  mLabelPosition = (AXISSide) xmltoi(node.getAttribute("labelPosition"), mLabelPosition);
  mLabelAlignment = (AXISAlignment) xmltoi(node.getAttribute("labelAlignment"), mLabelAlignment);
  mLabelOffset = xmltof(node.getAttribute("labelOffset"), mLabelOffset);

  child = node.getChildNode("legend");
  text  = child.getChildNode(0);
  if (! mLegend.fromXML(text)) {
    vwarning("Failed to retrieve label text for VisusBorderAxis");
    return false;
  }

  mDrawLegend = xmltobool(node.getAttribute("drawLegend"), mDrawLegend);
  mLegendPosition = (AXISSide) xmltoi(node.getAttribute("legendPosition"), mLegendPosition);
  mLegendAlignment= (AXISAlignment) xmltoi(node.getAttribute("legendAlignment"), mLegendAlignment);
  mLegendOffset = xmltof(node.getAttribute("legendOffset"), mLegendOffset);

  mDrawTicks = xmltobool(node.getAttribute("drawTicks"), mDrawTicks);
  mMajorTickLength = xmltof(node.getAttribute("majorTickLength"), mMajorTickLength);
  mMajorTickThickness = xmltof(node.getAttribute("majorTickThickness"), mMajorTickThickness);
  mMinorTickLength = xmltof(node.getAttribute("minorTickLength"), mMinorTickLength);
  mMinorTickThickness = xmltof(node.getAttribute("minorTickThickness"), mMinorTickThickness);
  mTickPosition = (AXISSide) xmltoi(node.getAttribute("tickPosition"), mTickPosition);

  mMinValue = xmltof(node.getAttribute("min"), mMinValue);
  mMaxValue = xmltof(node.getAttribute("max"), mMaxValue);
 
  mMajorTicks = xmltoi(node.getAttribute("majorTicks"), mMajorTicks);
  mMinorTicks = xmltoi(node.getAttribute("minorTicks"), mMinorTicks);
 
  XMLNode tickColor = node.getChildNode("TickColor");
  XMLNode color = tickColor.getChildNode(0);
  if (! mTickColor.fromXML(color)) {
    vwarning("Failed to retrieve tick color for VisusBorderAxis");
    return false;
  }

  return true;
}
