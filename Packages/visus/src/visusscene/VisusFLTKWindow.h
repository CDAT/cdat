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


#ifndef VISUSFLTKWINDOW_H
#define VISUSFLTKWINDOW_H

#include <cstdlib>

#include "fltk/GlWindow.h"
#include "VisusGuiBase.h"
#include "VisusOpenGLState.h"
#include "vfltk/FLTKNodeMenu.h"

/*! VisusFLTKWindow implements a basic OpenGL capable window in the
 *  fltk framework. This class is designed to be a wrapper around the
 *  VisusBaseGui class using the fltk library. 
 */
class VisusFLTKWindow : public fltk::GlWindow, public VisusGuiBase
{
public:
  
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  
  
  //! Constructor creating a window of the given dimensions and title
  VisusFLTKWindow(const char* title=NULL, int width=VisusOpenGLState::sDefaultWidth, 
                  int height=VisusOpenGLState::sDefaultHeight);

  //! Constructor creating a window of the given position
  VisusFLTKWindow(int x, int y, const char* title=NULL,
                  int width=VisusOpenGLState::sDefaultWidth, 
                  int height=VisusOpenGLState::sDefaultHeight);
  
  virtual ~VisusFLTKWindow();

  /***************************************************************
   ******       Event System                             *********
   **************************************************************/  
  
  //! The basic draw function
  virtual void draw();

  //! Function that handles all events
  virtual int handle(int event);

  //! Indicate that the display should be redrawn
  virtual void redisplay();

  //! Swap the frame buffers
  virtual void swapBuffers();

  //! Return the modifier of the last event
  virtual VisusModifier modifier();
  
  //! The keyboard callback function
  virtual void keyboard(unsigned char key, int x, int y);

  //! Overloaded destroy call to also remove the idle callback
  virtual void destroy();
  
  /***************************************************************
   ******       Window Properties                        *********
   **************************************************************/ 
  void cycleThroughBackgroundColors()
  {
    static int currentColorNum = 6;

    std::cout << "current color: " << currentColorNum << std::endl;

    switch(currentColorNum)
      {
      case 0:
	backgroundColor(.815, .839, 1);
	break;
      case 1:
	backgroundColor(.454, .466, .556);
	break;
      case 2:
	backgroundColor(.722, .725, .757);
	break;
      case 3:
	backgroundColor(.953, .957, 1.0);
	break;
      case 4:
	backgroundColor(.263, .263, .376);
	break;
      case 5:
	backgroundColor(.243, .243, .243);
	break;
      case 6:
	backgroundColor(.66, .66, .66);
	break;
      case 7:
	backgroundColor(1, 1, 1);
	break;
      default:
	backgroundColor(0, 0, 0);
	break;
      }
    currentColorNum ++;
    if(currentColorNum > 8)
      currentColorNum = 0;
    redisplay();
  }

  void backgroundColor(float r, float g, float b) {
    std::cout << "background color: " << r << " " << g << " " << b << std::endl;
    glClearColor(r,g,b,1);}
  
protected:

  //! Pointer to the last opened menu
  FLTKNodeMenuInterface* mOpenMenu;

  //! Popup the node menu for the given node 
  void popupMenu(pVisusGroup focus);

};


#endif

