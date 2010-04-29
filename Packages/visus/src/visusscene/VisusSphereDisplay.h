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


#ifndef VISUSSPHEREDISPLAY_H
#define VISUSSPHEREDISPLAY_H


#include "VisusGroup.h"
#include "VisusConsumer.h"
#include "VisusSmartPointer.h"
#include "VisusTexture.h"


class VisusSphereDisplay;
typedef VisusSmartPointer<VisusSpherDisplay> pVisusSphereDisplay;

class VIsusSphereDisplay : public VisusGroup, public VisusConsumer
{
public:

  static pVisusSpherDisplay instantiate();

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  

  //! Default constructor
  VisusSphereDisplay();

  //! Destructor
  virtual ~VisusSphereDisplay();
  

  /***************************************************************
   ******          Access Functions                      *********
   **************************************************************/
  
  //! Return the number of slices used
  int slices() {return mSlices;}

  //! Return the number of stacks used
  int stacks() {return mStacks;}

  //! Set the number of slices sued 
  void slices(int n) {mSlices = n;}

  //! Set the number of stacks used
  void stacks(int n) {mStacks = n;}
  

  virtual void display(VisusTransformation3D model_view_3D = VisusTransformation3D(),
                       VisusTransformation2D model_view_2D = VisusTransformation2D(),
                       bool lock_graph = true);
  
  int connectInput(pVisusProducer producer);

private:

  VisusTexture mTexture;
};

#endif
