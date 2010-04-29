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


#ifndef VISUSTICKMARKS_H
#define VISUSTICKMARKS_H

#include "VisusColor.h"
#include "VisusText.h"
#include "VisusGroup.h"
#include "VisusBoundingBox.h"
#include "VisusSmartPointer.h"
#include "VisusBorderAxis.h"
#include <vector>
#include <string>

enum TMAxis {
	TM_X_AXIS=0,
	TM_Y_AXIS,
	TM_Z_AXIS
};

class VisusTickMarks;
typedef VisusSmartPointer<VisusTickMarks> pVisusTickMarks;

class VisusTickMarks : public VisusGroup
{
public:
  
#ifndef VISUS_DISABLE_FLTK
  
  friend class TickMarksMenu;
#endif


  static pVisusTickMarks instantiate();
  
  //! Default Constructor
  VisusTickMarks();
  
  //! Destructor
  ~VisusTickMarks() {}
  
  //! Return an info string identifying the node
  virtual std::string infoString() const {return std::string("Tick marks");}

  /***************************************************************
   ******       Set/Get Functions                        *********
   **************************************************************/  
  
  //! Function which sets the axis to display tick mark bar on
  void direction(const TMAxis axis);
  
  //! Function to retrieve the axis configuration settings
  VisusBorderAxis axis() const { return mAxis; }

  //! Function to set axis configuration settings
  void axis(const VisusBorderAxis& axis) { mAxis = axis; }
  
  //! Display the bounding box of the axis we label
  void displayBoundingBox() const;
  
protected:
  void toXMLLocalVariables(XMLNode& parent);
  bool fromXMLLocalVariables(XMLNode& node);
  
  //! Function to draw the TickMarks and any children
  virtual void display3D(VisusTransformation3D model_view_3D = VisusTransformation3D());

private:
  
  //! Return the length of the specified axis
  float getDimension(const TMAxis axis);
  
  //! Determines the edge for the axis which is most viewable
  void getMostVisible(const VisusTransformation3D& modelView);
  
  void printMatrix(const VisusTransformation3D& view);
  
  /***************************************************************
   ******       Default Values                           *********
   **************************************************************/  
  static const float         DEFAULT_LENGTH;
  static const float         DEFAULT_WIDTH;

  static const char        DEFAULT_FORMAT_STRING[10];
  static const int         DEFAULT_LABEL_SIZE;
  static const float       DEFAULT_LABEL_OFFSET;

  static const int           DEFAULT_LEGEND_SIZE;
  static const AXISAlignment DEFAULT_LEGEND_ALIGNMENT;
  static const float         DEFAULT_LEGEND_OFFSET;

  static const unsigned char DEFAULT_BORDER_COLOR[4];

  static const float  DEFAULT_MAJOR_LENGTH;
  static const float  DEFAULT_MAJOR_WIDTH;
  static const float  DEFAULT_MINOR_LENGTH;
  static const float  DEFAULT_MINOR_WIDTH;
  static const float  DEFAULT_MIN_VALUE;
  static const float  DEFAULT_MAX_VALUE;
  static const int    DEFAULT_MAJOR_TICKS;
  static const int    DEFAULT_MINOR_TICKS;

  /***************************************************************
   ******       Draw Functions                          *********
   **************************************************************/  
  void render();

  void renderTick(const float start[3],
                  const float axisdir[3], // must be normalized
                  const float tickdir[3], // must be normalized
                  const float length,     // units of 'tickdir' vector
                  const float thickness   // as fraction of length
                  );

  void renderLabel(const float   origin[3],
                   const float   dir[3],    // normalized direction to offset from start
                   VisusText& label,        // contains sprintf string for label
                   const char* text,
                   const float   offset,
                   const int     canvasWidth,
                   const int     canvasHeight);

  void renderLabelForTick(const float   start[3],
                          const float   tickdir[3],// must be normalized
                          const float   length,    // units of 'tickdir'
                          VisusText&  label,// tick template 
                          const float   tickVal,   // Value to display for the tick
                          const int     canvasWidth,
                          const int     canvasHeight);

  void renderTickMarks(const int canvas_width, const int canvas_height);

  void renderLegend(const int canvas_width, const int canvas_height);

  /***************************************************************
   ******       Geometry                                 *********
   **************************************************************/

  // These values are operated on by the active OpenGL state
  // at time of rendering, and thus are subject to multiple
  // transformations.

  VisusBoundingBox mBBox;
  TMAxis mTMAxis;
  float mOrigin[3];
  float mDirection[3];      // "Up" vector is mDirection X mTickDirection
  float mTickDirection[3];
  float mLength;            // in units of normalized mDirection vector

  /***************************************************************
   ******        Appearance                              *********
   **************************************************************/    

  //! Axis settings
  VisusBorderAxis mAxis;

  /***************************************************************
   ******       Preset OpenGL Tranformations              *********
   **************************************************************/    

  //! OpenGl matrix to draw text left to right (identity)
  static const float LEFT_RIGHT_MATRIX[16];

  //! OpenGl matrix to draw text bottom to top
  static const float BOTTOM_TOP_MATRIX[16];

  //! OpenGl matrix to draw text top to bottom 
  static const float TOP_BOTTOM_MATRIX[16];

  //! OpenGl matrix to draw text right to left (upside-down)
  static const float RIGHT_LEFT_MATRIX[16];

  //! The Axis that Tick Marks Go In For Specified Index Axis
  static const TMAxis TICK_AXIS[3];
};

#endif
