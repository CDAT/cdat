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


#ifndef VISUSLINEGRAPH_H
#define VISUSLINEGRAPH_H

#include "VisusGraph.h"
#include "VisusConsumer.h"
#include "VisusColor.h"

struct XMLNode;
class VisusBlockData;
class VisusBoundingBox;


class VisusLineGraph;
typedef VisusSmartPointer<VisusLineGraph> pVisusLineGraph;


class VisusLineGraph : public VisusGraph, public VisusConsumer
{
 public:

  static pVisusLineGraph instantiate();
	 
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  
  
  //! Default constructor
  VisusLineGraph();

  //! Destructor
  virtual ~VisusLineGraph();  

  //! Set the number of lines to display 
  bool numberOfLines(const int n);

  //! Get the number of lines to display
  int numberOfLines() const { return mNrOfInputs; }

  //! Set point size
  bool pointSize(const float size);

  //! Set the color of given line
  bool lineColor(const int i, const VisusColor& color);

  //! Set line width
  bool lineWidth(const float width);

  //! Auto-adjust box min / max bounds based on data
  void autoAdjustMinMax(const BBAxis axis, bool autoAdjustOn=true);

protected:

  void toXMLLocalVariables(XMLNode& node);
  bool fromXMLLocalVariables(XMLNode& node);

 /***************************************************************
  ******       Draw Functions                           *********
  **************************************************************/    
  virtual void display2D(VisusTransformation2D model_view_2D = VisusTransformation2D());  

  virtual bool renderInsideBox(const float boxWidth, const float boxLength);

  //! Functor used to draw the points in the line graph
  class DrawFunctor : public GraphHelper::DataFunctor
  {
  public:
    DrawFunctor(const float boxWidth, const float boxLength,
                const VisusBorderAxis& x, const VisusBorderAxis& y);
  protected:
    void process(float x, float y);
  private:
    float xMin,xMax,yMin,yMax,xfactor,yfactor;
  };

  typedef std::vector<VisusBlockData*> BlockDataList;

private:

  bool renderLine(const float boxWidth, const float boxLength,
                  const VisusColor& color, const VisusBlockData& data);

  //! Individual line colors, one-to-one with Inputs
  std::vector<VisusColor> mLineColor;

  //! Auto-adjust box min/max bounds based on current data
  bool mAutoAdjustX;
  bool mAutoAdjustY;

  //! Point Size
  float mPointSize;

  //! Line Width
  float mLineWidth;

  //! Line graph data
  BlockDataList mData;
};

#endif
