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


#ifndef VISUSHISTOGRAM_H
#define VISUSHISTOGRAM_H

#include "VisusGraph.h"
#include "VisusConsumer.h"
#include "VisusColor.h"
#include "VisusBlockData.h"

struct XMLNode;
class VisusBoundingBox;


class VisusHistogram;
typedef VisusSmartPointer<VisusHistogram> pVisusHistogram;


class VisusHistogram : public VisusGraph, public VisusConsumer
{
 public:

  static pVisusHistogram instantiate();
	 
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  
  
  //! Default constructor
  VisusHistogram();

  //! Destructor
  virtual ~VisusHistogram();  

  //! Set the color of bar
  bool barColor(const VisusColor& color);

  //! Set whether to fill bar or make hollow
  bool barFill(const bool fill);

  //! Set width of bar as percentage of length of axis
  bool barWidth(const float width);

  //! Auto-adjust box min / max bounds based on data
  void autoAdjustMinMax(bool autoAdjustOn) { mAutoAdjust = autoAdjustOn; }

  //! Auto-adjust box size for even distribution of bars
  void evenDistribution(bool turnOn) { mEvenDist = turnOn; }


protected:

  void toXMLLocalVariables(XMLNode& node);
  bool fromXMLLocalVariables(XMLNode& node);

 /***************************************************************
  ******       Draw Functions                           *********
  **************************************************************/    
  virtual void display2D(VisusTransformation2D model_view_2D = VisusTransformation2D());  

  virtual bool renderInsideBox(const float boxWidth, const float boxLength);

  void processEvenDistribution(VisusTransformation2D model_view_2D, const int numHistogramItems);

private:

  //! Auto-adjust box min/max bounds based on current data
  bool mAutoAdjust;

  //! Even distribution of bars
  bool mEvenDist;
  int  mEvenDistNumPixelsPerBar;
  int  mEvenDistNumPixelsForBox;

  //! Color of bar
  VisusColor mBarColor;

  //! Width of bar
  float mBarWidth;

  //! Whether or not to fill bar
  bool mBarFill;

  //! Histogram data
  VisusBlockData mData;
};

#endif

