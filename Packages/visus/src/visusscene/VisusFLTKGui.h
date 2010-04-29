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


#ifndef VISUSFLTKGUI_H
#define VISUSFLTKGUI_H

#include <vector>

#include "VisusGroup.h"

class VisusFLTKWindow;

/*! Master FLTK gui that allows the creation and destruction of
 *  multiple VisusFLTKWindows. The class provides a very thin layer
 *  around the standard fltk functions like fltk::run() that relieves
 *  us from wrapping any fltk functionality in python. The class has
 *  two modes of operation. First, the standard fltk mode. One creates
 *  one or several fltk windows (which are automatically shown) and
 *  then calls the mainloop() which will not return as long as windows
 *  are open. Second, slave-mode, where one repeatedly calls update to
 *  process the next event. This mode is designed to run this gui as a
 *  slave to another gui by smuggeling the update call into the idle
 *  loop of the master. Note that the window creation and the update
 *  calls must be executed from the main thread to properly capture
 *  window events (no threaded gui). 
 */
class VisusFLTKGui 
{
public:
  
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  
  
  //! Create a master gui stub without windows 
  VisusFLTKGui();

  //! Destroy the master gui
  ~VisusFLTKGui();


   /***************************************************************
   ******       Interface                                 *********
   **************************************************************/  
 
  //! Create a window displaying the scenegraph given by its root 
  int createWindow(pVisusGroup root);

  //! Return a pointer to the i'th window
  const VisusFLTKWindow& window(int i=0);

  //! The main event loop
  /*! This function will ultimatley call fltk::run() and will only
   *  return once all windows have been closed
   */
  int mainLoop();

  //! A single round of the event loop
  /*! This function will eventually call fltk::wait(sUpdateIntervall)
   *  to wait the given number of seconds for an event and processes
   *  it once such an event occurs. It will also check all active
   *  windows whether they are still active moving them to the trash
   *  if necessary.
   *  @return: 1 if there still exist active windows and further 
   *           updates might be necessary and 0 otherwise.
   */  
  int update();

private:

  //! Number of seconds the gui will sleep if no events occur
  //! before checking messages
  static const float sUpdateInterval;

  //! Collection of all active windows
  std::vector<VisusFLTKWindow*> mActiveWindows;

  //! Garbage collection of all inactive windows
  std::vector<VisusFLTKWindow*> mTrash;
};

#endif
