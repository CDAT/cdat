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

#ifndef VISUSGUITHREAD_H
#define VISUSGUITHREAD_H

#include "VisusThread.h"
#include "VisusGroup.h"

template<class GuiClass>
class VisusGuiThread 
{
public:

  //! Constructor
  VisusGuiThread();

  //! Destructor
  ~VisusGuiThread();

  //! Create a window to display the scene graph 
  void createWindow(pVisusGroup root);

  //! Checks whether the gui is still running
  bool isActive();
  
  //! Run the mainloop in a blocking way
  void initThread();
  
private:

  //! Function to start the thread
  static void* threadStart(void *data);

  //! The thread running the mainloop of the GuiClass
  VisusThread* mThread;

  //! Instance of the GuiClass 
  GuiClass* mGui;

  //! Member call to rnu the main loop
  void mainloop();
};


template <class GuiClass>
VisusGuiThread<GuiClass>::VisusGuiThread() : mThread(NULL), mGui(NULL)
{
}
  
template <class GuiClass>
VisusGuiThread<GuiClass>::~VisusGuiThread() 
{
  if (mGui != NULL)
    delete mGui;

  if (mThread != NULL)
    delete mThread;
}

template <class GuiClass>
void VisusGuiThread<GuiClass>::createWindow(pVisusGroup root)
{
  mGui->createWindow(root);
}

template <class GuiClass>
bool VisusGuiThread<GuiClass>::isActive()
{
  mGui->destroyInactive();

  if (mThread->status() == FINISHED)
    return true;
  else 
    return false;
}

template <class GuiClass>
void* VisusGuiThread<GuiClass>::threadStart(void* data)
{
  ((VisusGuiThread*)data)->mainloop();

  return NULL;
}

template <class GuiClass>
void VisusGuiThread<GuiClass>::mainloop()
{
  //fltk::lock();

  // This function will be called from the thread constructor and thus
  // cannot access mThread as the object might not be fully formed yet
  //mThread->status(WORKING);

  mGui = new GuiClass();

  mGui->mainloop();

  //mThread->status(FINISHED);
}


template <class GuiClass>
void VisusGuiThread<GuiClass>::initThread() 
{
  mThread = new VisusThread(VisusGuiThread<GuiClass>::threadStart,this);
  
  while (mGui == NULL)
    sleepmillisec(100); 
  
  mThread->status(WORKING);
}
  



#endif
