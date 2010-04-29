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


#ifndef VISUSCOLORBAR_H
#define VISUSCOLORBAR_H

#include <vector>

#include "VisusBorderBox2D.h"
#include "VisusSmartPointer.h"


class VisusBoundingBox;

class VisusColorBar;
typedef VisusSmartPointer<VisusColorBar> pVisusColorBar;


class VisusColorBar : public VisusBorderBox2D
{
 public:

  static pVisusColorBar instantiate();

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  
  
  //! Default constructor
  VisusColorBar();

  //! Destructor
  virtual ~VisusColorBar() {}

  //! Retrieve the border axis configurable variables
  VisusBorderAxis axis() { return mXAxis; }

  //! Set the border axis configurable variables
  void axis(const VisusBorderAxis& axis) { mXAxis = axis; }

  //! Set length of the color bar
  void length(float l) { mLength = l; }

  //! Set width of the color bar
  void width (float w) { mWidth = w; }

 virtual void displayBoundingBox() {}

protected:

  /***************************************************************
   ******       Draw Functions                           *********
   **************************************************************/  
  
   virtual void display2D(VisusTransformation2D model_view_2D = VisusTransformation2D());  

  virtual bool renderInsideBox(const float boxWidth, const float boxLength);

};

#endif
