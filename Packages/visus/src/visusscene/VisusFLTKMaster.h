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

#ifndef VISUSFLTKMASTER_H
#define VISUSFLTKMASTER_H

#include <vector>

#include "fltk/Window.h"
#include "fltk/run.h"
#include "VisusGuiInterface.h"
#include "VisusGuiThread.h"

class VisusFLTKMaster;
class VisusFLTKWindow;

typedef VisusGuiThread<VisusFLTKMaster> VisusFLTKThread;

//! Implementation of a VisusGuiInterface for FLTK
class VisusFLTKMaster : public VisusGuiInterface, public fltk::Window
{
public:

  //! Default constructor 
  VisusFLTKMaster();
  
  //! Destructor
  virtual ~VisusFLTKMaster();

  
  //! The function calling the mainloop
  virtual void mainloop();
  
  //! The message handling function
  int processMessages();

  //! Destroy all inactive windows and potentially the main window 
  /*! Calling destroyInactive will check for active windows and remove
   *  inactive ones. If no active windows are found the function will
   *  call destroy and thus end the fltk::run() mainlook. It returns the
   *  number of active windows.  
   *  @return Number of active windows
   */
  int destroyInactive();
  
  

private:

  //! The list of windows we have created
  std::vector<VisusFLTKWindow*> mActiveWindows;

  //! The idle call back to process asynchronysly process msgs
  static void idleCallback(void* v);

  //! Overloaded destroy call to also remove the idle callback
  virtual void destroy();
};


#endif

