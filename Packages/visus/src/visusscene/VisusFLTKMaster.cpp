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


#include "VisusFLTKMaster.h"
#include "VisusFLTKWindow.h"

template class VisusGuiThread<VisusFLTKMaster>;

VisusFLTKMaster::VisusFLTKMaster() : fltk::Window(0,0,"VisusFLTKMaster")
{
  fltk::add_idle(VisusFLTKMaster::idleCallback,this);  
  this->show();
}


VisusFLTKMaster::~VisusFLTKMaster()
{
  std::vector<VisusFLTKWindow*>::iterator it;

  // Destroy all the remaining active windows 
  for (it=mActiveWindows.begin();it!=mActiveWindows.end();it++) {
    delete *it;
  }

  this->destroy();  
}

void VisusFLTKMaster::mainloop() 
{
  fltk::run();
}


int VisusFLTKMaster::processMessages()
{
  VisusMessage msg;

  if (mMessageQueue.empty()) 
    return 1;

  if (mMutex.lock() == 0) {
    vwarning("Could not lock mutex for gui message queue. Message ignored.");
    return 0;
  }

  if (!mMessageQueue.empty()) {
    msg = mMessageQueue.front();
    mMessageQueue.pop();

    if (mMutex.unlock() == 0) 
      vwarning("Could not unlock mutex for gui messaga queue.");
  }
  else {
    if (mMutex.unlock() == 0) {
      vwarning("Could not unlock mutex for gui messaga queue.");
      return 0;
    }

    return 1;
  }  


  pVisusGroup* root;

  switch (msg.id) {
    
  case VISUS_CREATE_WINDOW: {
    
    root = (pVisusGroup*)msg.buffer;
    VisusFLTKWindow* win = new VisusFLTKWindow("ViSUS 2.0");
    win->setRoot(*root);
    win->show();
    mActiveWindows.push_back(win);

    break;
  }
  case VISUS_DESTROY_INACTIVE: {
    
    destroyInactive(); 
    break;
  }
   
  default:
    break;
  }

  return 1;
}

int VisusFLTKMaster::destroyInactive()
{
  std::vector<VisusFLTKWindow*>::iterator it;
  
  it=mActiveWindows.begin();
  while (it != mActiveWindows.end()) {
    if ((*it)->context() == 0) {
      std::iter_swap(it,mActiveWindows.end()-1);
      delete mActiveWindows.back();
      mActiveWindows.pop_back();
    }
    else 
      it++;
  }

  if (mActiveWindows.empty()) 
    destroy();

  return mActiveWindows.size();
}

  

void VisusFLTKMaster::idleCallback(void* v) 
{
  VisusFLTKMaster* win = (VisusFLTKMaster*)v;

  //fprintf(stderr,"VisusFLTKMaster::idle() \n");
  win->processMessages();
  //this->update_child(*win);
//fltk::update();

  //fltk::lock();
  //fprintf(stderr,"Is mainthread  %d\n",fltk::in_main_thread());
  //fltk::unlock();

  fltk::wait(0.1);
}

void VisusFLTKMaster::destroy()
{
  fltk::remove_idle(VisusFLTKMaster::idleCallback,this);  
  fltk::Window::destroy();
}
