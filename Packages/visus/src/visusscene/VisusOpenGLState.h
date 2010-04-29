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


#ifndef VISUSOPENGLSTATE_H
#define VISUSOPENGLSTATE_H

#if WIN32
#include <windows.h>
#endif

#include <vector>
#include "glew.h"

#include "xmlParser.h"

//! This class stores the current OpenGL state
/*! This class will ultimately store all necessary aspects of the
 *  OpenGL state. In particular, it stores the viewport and projection
 *  matrix independent of OpenGL calls. This allows using
 *  VisusTranformation3D::translate/rotate calls without an active
 *  OpenGL context. To easily maintain the correct default values, the
 *  OpenGL state also defines the default window parameter, as well as
 *  viewing volume.
 */
class VisusOpenGLState {

public:

  /***************************************************************
   ******       Static default values                    *********
   **************************************************************/    
  
  /*! The static default values will not only influence the standard
   *  gui that is created but also determine the glViewport and
   *  glProjection matrix before the first draw call. Thus the
   *  defaults are carefuly chosen to algin with the default
   *  construction of the scence and will *not* work un-altered if the
   *  default scene has changed.
   */
  

  //! Width of the default window
  static const int sDefaultWidth;

  //! Height of the default window
  static const int sDefaultHeight;
  
  //! Default camera position
  static const double sDefaultCameraPosition[3];

  //! Default viewing direction
  static const double sDefaultView[3];

  //! Default up direction
  static const double sDefaultUp[3];

  //! Default viewing volume
  static const double sDefaultViewVolume[6];

  //! Default viewport
  static const GLint sDefaultViewport[4];

  //! Default projection
  static const GLdouble sDefaultProjection[16];

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  

  //! Default constructor
  VisusOpenGLState();
  
  //! Copy constructor
  VisusOpenGLState(const VisusOpenGLState& state);

  //! Destructor
  virtual ~VisusOpenGLState() {}


  //! Assignment operator
  VisusOpenGLState& operator=(const VisusOpenGLState& state);

  //! Return the current projection
  GLdouble const *projection() const {return mProjection;}

  //! Return the current viewport
  GLint const* viewport() const {return mViewport;}

  //! Fetch the current state from OpenGL (needs a valid context)
  void fetchState();
  
  void printState();

  //! Build instance data from XML data
  bool fromXML(XMLNode& node) {return true;}

  //! Build XML data from instance data
  void toXML(XMLNode& parent) const {}

private:

  //! The OpenGL projection matrix;
  GLdouble mProjection[16];

  //! The OpenGL viewport
  GLint mViewport[4];
};

#endif
