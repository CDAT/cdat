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


#ifndef VisusBorderAxis_H
#define VisusBorderAxis_H

#include "VisusColor.h"
#include "VisusText.h"

//! Border Axis
enum BBAxis 
{
  BB_X_AXIS=0,
  BB_Y_AXIS
};

//! Type to indicate on which "side" to draw the ticks, legend, etc.
enum AXISSide {
  AXIS_LOW = 0,    //! Draw on low axis 
  AXIS_HIGH,       //! Draw on high axis
  AXIS_BOTH,       //! Draw on both low and high axis
  AXIS_NULL,
};

//! Type to indicate where to place (align) legend text
enum AXISAlignment {
  AXIS_CENTER_ALIGN =0,
  AXIS_LEFT_ALIGN,
  AXIS_RIGHT_ALIGN,
};

enum BBTickLine
{
  BB_NONE=0,
  BB_DASHED,
  BB_SOLID
};

class VisusBorderBox2D;
class VisusTickMarks;

class VisusBorderAxis
{
public:

  static const char* XML_TAG;

  //! Default Constructor
  VisusBorderAxis();

  //! Copy Constructor
  VisusBorderAxis(const VisusBorderAxis& config);

  //! Destructor
  ~VisusBorderAxis();

  //! Assignment operator
  VisusBorderAxis& operator=(const VisusBorderAxis& config);

  //! Enable axis border ticks, labels, and legend
  void enable();

  //! Disable axis border ticks, labels, and legend
  void disable();


  // --  Axis Legend --

  
  //! Return whether we draw the legend
  bool drawLegend() const { return mDrawLegend; }

  //! Return the orientation of legend text
  VISUS_TEXT_ORIENTATION legendOrientation() const { return mLegend.orientation(); }

  //! Return the text value of the legend
  const char* legendText() const { return mLegend.text();}

  //! Return the font of the legend
  VisusFont legendFont() const {return mLegend.font();}

  //! Return the position of the legend (AXISSide)
  AXISSide legendPosition() const {return mLegendPosition;}

  //! Return the alignment of the legend (AXISAlignment)
  AXISAlignment legendAlignment() const {return mLegendAlignment;}

  //! Return the offset of the legend
  float legendOffset() const {return mLegendOffset;}


  //! Toggle on/off drawing of legend
  void drawLegend(bool enable) { mDrawLegend = enable; }

  //! Set the orientation of legend text
  void legendOrientation(VISUS_TEXT_ORIENTATION orient) { mLegend.orientation(orient); }

  //! Set the text value of the legend
  void legendText(const char* text);

  //! Set the font of the legend
  void legendFont(const VisusFont& font);

  //! Set the position of the legend (AXISSide)
  void legendPosition(AXISSide side) {mLegendPosition = side;}

  //! Set the alignment of the legend (AXISAlignment)
  void legendAlignment(AXISAlignment align) {mLegendAlignment = align;}

  //! Set the offset of the legend
  void legendOffset(float offset) {mLegendOffset = offset;}

 
  // -- Tick Labels --

  //! Toggle on/off drawing of labels
  bool drawLabels() const { return mDrawLabels; }

  //! Set the text value format string of tick labels
  //! This needs to be in the form of a sprintf call.. (ie %0.1f etc)
  const char* labelText() const {return mLabel.text();}

  //! Set the font of the tick labels
  VisusFont labelFont() const {return mLabel.font();}

  //! Set the position of the tick labels (AXISSide)
  AXISSide labelPosition()  const{return mLabelPosition;}

  //! Set the orientation of labels text
  VISUS_TEXT_ORIENTATION labelOrientation() const {return mLabel.orientation(); }
  
  //! Set the alignment of the tick labels (AXISAlignment)
  AXISAlignment labelAlignment() const {return mLabelAlignment;}

  //! Get the offset of the legend
  float labelOffset() const { return mLabelOffset; }
  
  //! Toggle on/off drawing of labels
  void drawLabels(bool enable) { mDrawLabels = enable; }

  //! Set the text value format string of tick labels
  //! This needs to be in the form of a sprintf call.. (ie %0.1f etc)
  void labelText(const char* text);

  //! Set the font of the tick labels
  void labelFont(const VisusFont& font);

  //! Set the position of the tick labels (AXISSide)
  void labelPosition(AXISSide side) {mLabelPosition = side;}

  //! Set the orientation of labels text
  void labelOrientation(VISUS_TEXT_ORIENTATION orient) { mLabel.orientation(orient); }
  
  //! Set the alignment of the tick labels (AXISAlignment)
  void labelAlignment(AXISAlignment align) {mLabelAlignment = align;}

  //! Set the offset of the tick labels
  void labelOffset(float offset) {mLabelOffset = offset;}
  

  //! Set the tick labels, this overrides auto-tick labels via min/max value
  void tickLabels(int numLabels, std::string labels[]);

  //! Set the tick labels, this overrides auto-tick labels via min/max value
  void tickLabels(const std::vector<std::string>& labels);

  //! Get whether or not axis has predefined labels
  bool hasTickLabels() const { return mTickLabels.size() > 0; }

  // -- Ticks --
  
  //! Toggle on/off drawing of tick marks
  void drawTicks(bool enable) { mDrawTicks = enable; }

  //! Set the tick length of the major ticks
  void majorTickLength(float length) {mMajorTickLength = length;}

  //! Get the tick length of the major ticks
  float majorTickLength() {return mMajorTickLength;}


  //! Set the tick thickness of the major ticks
  void majorTickThickness(float thick) {mMajorTickThickness = thick;}

  //! Get the tick thickness of the major ticks
  float majorTickThickness() const {return mMajorTickThickness;}

  //! Set the tick length of the minor ticks
  void minorTickLength(float length) {mMinorTickLength = length;}

  //! Get the tick length of the minor ticks
  float minorTickLength() {return mMinorTickLength;}

  //! Set the tick thickness of the major ticks
  float minorTickThickness() {return mMinorTickThickness;}

  //! Set the tick thickness of the major ticks
  void minorTickThickness(float thick) {mMinorTickThickness = thick;}

  //! Set the position of the ticks (AXISSide)
  void tickPosition(AXISSide side) { mTickPosition = side; }

  //! Set the minValue aka label value for first tick
  void minValue(float f) {mMinValue = f;}

  //! Get minValue for labels
  float minValue() const { return mMinValue; }

  //! Set the maxValue aka label value for last tick
  void maxValue(float f) {mMaxValue = f;}

  //! Get maxValue for labels
  float maxValue() const { return mMaxValue; }

  //! Set the number of major ticks to draw
  void majorTicks(int n) {mMajorTicks = n;}
  int majorTicks() const { return mMajorTicks; }

  //! Set the number of minor ticks between major ticks to draw
  void minorTicks(int n) {mMinorTicks = n;}
  int minorTicks() const { return mMinorTicks; }

  //! Set the color of tick marks
  void tickColor(const VisusColor& color);


  // --- Bounding Bos ---
  VisusBoundingBox getDrawBox() const { return mDrawBox; } 


  // --- XML ---

  //! Build instance data from XML data
  bool fromXML(XMLNode& node);

  //! Build XML data from instance data
  void toXML(XMLNode& node) const;


  void renderTickMarks(const float borderWidth, const float borderLength, 
		                   const BBAxis axis, const BBTickLine tickLineType);
  void renderLabels(const int canvasWidth, const int canvasHeight);
  void renderLegend(const int canvasWidth, const int canvasHeight);


protected:
  float mWidth;
  float mLength;
  
private:
	
  void drawTick(const float low, const float high, const float axisPosition, 
                const BBTickLine tickLineType);

  // --- Label variables ---

  //! Whether or not to draw the labels    
  bool mDrawLabels;

  //! Text to use for the labels
  VisusText mLabel;

  //! Relative position of the labels
  AXISSide mLabelPosition;

  //! Alignement of the labels relative to the tick marks
  AXISAlignment mLabelAlignment;

  //! The offset between tick marks and labels
  float mLabelOffset;

  // --- Legend (aka the title of the axis) ----
 
  //! Whether or not to draw the legend
  bool mDrawLegend;
 
  //! Title (text) of the color map
  VisusText mLegend;
 
  //! Relative position of the legend
  AXISSide mLegendPosition;

  //! Alignment of the legend
  AXISAlignment mLegendAlignment;
  
  //! The offset between labels/top and the legend
  float mLegendOffset;

  /***************************************************************
   ******        Appearance                              *********
   **************************************************************/    

  // --- Ticks ---

  //! Whether or not to draw the ticks
  bool mDrawTicks;

  //! Length of the major tick marks in percent of the width
  float mMajorTickLength;

  //! Length of the minor tick marks in percent of the major ones
  float mMinorTickLength;

  //! Thickness of the major tick marks
  float mMajorTickThickness;

  //! Thickness of the minor tick marks
  float mMinorTickThickness;

  //! Relative position of the ticks
  AXISSide mTickPosition;

  //! Minimal function value
  float mMinValue;
  
  //! Maximal function value
  float mMaxValue;
  
  //! Number of major tick marks >= 2
  int mMajorTicks;
  
  //! Number of minor tick marks 
  int mMinorTicks;

  //! Color of the border as well as the tick marks
  VisusColor mTickColor;

  // --- Tick Labels ---

  //! Tick labels which override min/max auto-labeling of ticks
  std::vector<std::string> mTickLabels;

  //! Bounding Box used for drawing
  VisusBoundingBox mDrawBox;
  int              mDrawAxis;
  int              mDrawOther;

 /***************************************************************
  ******       Default Values                           *********
  **************************************************************/  
  static const char DEFAULT_FORMAT_STRING[10];
  static const int DEFAULT_LABEL_SIZE;
  static const AXISSide DEFAULT_LABEL_POSITION;
  static const AXISAlignment DEFAULT_LABEL_ALIGNMENT;
  static const float DEFAULT_LABEL_OFFSET;
  static const int DEFAULT_LEGEND_SIZE;
  static const AXISSide DEFAULT_LEGEND_POSITION;
  static const AXISAlignment DEFAULT_LEGEND_ALIGNMENT;
  static const float DEFAULT_LEGEND_OFFSET;
  static const float DEFAULT_MAJOR_LENGTH;
  static const float DEFAULT_MAJOR_THICKNESS;
  static const float DEFAULT_MINOR_LENGTH;
  static const float DEFAULT_MINOR_THICKNESS;
  static const AXISSide DEFAULT_TICK_POSITION;
  static const float DEFAULT_MIN_VALUE;
  static const float DEFAULT_MAX_VALUE;
  static const int DEFAULT_MAJOR_TICKS;
  static const int DEFAULT_MINOR_TICKS;

  friend class VisusBorderBox2D;
  friend class VisusTickMarks;
};

//===============================================================================
// Return the number of pixels used to draw the given length of the box along X
//===============================================================================
#ifndef SWIG
#include <cmath>

inline double numPixels(const GLdouble model[], const GLdouble projection[], 
			const GLint viewport[], 
                        const float length, const int direction /* 0=x, 1=y */)
{
  GLdouble c1[3], c2[3], d[3];

  gluProject(0,0,0,model,projection,viewport,&c1[0],&c1[1],&c1[2]);
  switch(direction)
  {
  case 0:
    gluProject(length,0,0,model,projection,viewport,&c2[0],&c2[1],&c2[2]);
    break;
  case 1:
    gluProject(0,length,0,model,projection,viewport,&c2[0],&c2[1],&c2[2]);
    break;
  }

  for (int i=0; i<3; ++i)
    d[i] = c2[i] - c1[i];

  return sqrt(d[0]*d[0] + d[1]*d[1] + d[2]*d[2]);
}
#endif

#endif
