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


#include "VisusFLTKWindow.h"
#include "fltk/events.h"
#include "fltk/run.h"
#include <iostream>
#include "vfltk/FltkMenus.h"

using namespace fltk;


//! Idle callback
void idle_callback(void* v) 
{
  //fprintf(stderr,"fltk::idle_callback   %d\n",v);

  VisusFLTKWindow *win = (VisusFLTKWindow*)v;
  pVisusGroup root;

  root = win->getRoot();

  //fprintf(stderr,"fltk::idle_callback\n");
  
  if ((root != NULL) && (root->readClearDirty())) {
    //fprintf(stderr,"fltk::idle_callback\n");
    win->redisplay();
  }
  else {
    //sleepmillisec(10);
    fltk::wait(0.01);
  }
}


VisusFLTKWindow::VisusFLTKWindow(const char* title, int width, int height) :
  GlWindow(width,height,title), VisusGuiBase(width,height), mOpenMenu(NULL)
{
  fltk::add_idle(idle_callback,this);  
  resizable(this);
}

VisusFLTKWindow::VisusFLTKWindow(int x, int y, const char* title, int width, int height) :
  GlWindow(x,y,width,height,title), VisusGuiBase(width,height), mOpenMenu(NULL)
{
  fltk::add_idle(idle_callback,this);  
  resizable(this);
}

VisusFLTKWindow::~VisusFLTKWindow()
{
  if (fltk::has_idle(idle_callback,this))
    fltk::remove_idle(idle_callback,this);

  destroy();
}

void VisusFLTKWindow::draw()
{
  glViewport(0, 0, w(), h());
  mWindowWidth = w();
  mWindowHeight = h();

  // If the OpenGL context is not currently valid
  if (!valid()) { 
    initOpenGL(); // We (re-)initialize OpenGL
    vglerror();
  }
  
  
  //fprintf(stderr,"VisusFLTKWindow::draw()\n");
  display();
}

int VisusFLTKWindow::handle(int event)
{
  // fltk handles *all* events in this function. To be compatible with
  // the more glut-like style of the VisusBaseGui we need to split up
  // the events as glut does
  
  int x = event_x();
  int y = event_y();

  //fprintf(stderr,"VisusFLTKWindow::handle  even: %d  modifier %d\n",event,modifier());

  switch (event) {
    
  case NO_EVENT:
    return 0;
  case PUSH: {// Mouse button push
    switch (event_button()) {
    case LeftButton:
    case MiddleButton:
    case RightButton:
      mouse(event_button()-1,VISUS_MOUSE_DOWN,x,y);
      break;
    default:
      break;
    }
    break;
  }
  case RELEASE: {// Mouse button release
    switch (event_button()) {
    case LeftButton:
    case MiddleButton:
    case RightButton:
      mouse(event_button()-1,VISUS_MOUSE_UP,x,y);
      break;
    default:
      break;
    }
    break;
  }
  case DRAG:
    motion(x,y);
    break;
  case KEY:
	keyboard(event_key(),x,y);
    break;
  case FOCUS:
    //take_focus();
    //return 0;
    break;
  case HIDE:
    // This case is misleadingly called "HIDE". This event is
    // triggered by closing (not minimizing) the window. In the C++
    // case this triggers a destroy event as the last window becomes
    // invisible. However, calling this from python somehow avoids
    // this and we must call destroy explicitly.
    destroy();
    break;
  default: 
    // If we did not handle the event we return 0 to let someone else
    // try 
    //return 0;
    return fltk::GlWindow::handle(event);
  }
  
  // We return 1 to indicate that we handled the event
  return 1;
}

void VisusFLTKWindow::redisplay()
{
  redraw();
}

// FLTK automatically swaps the buffers after the draw routine
void VisusFLTKWindow::swapBuffers()
{
}

VisusModifier VisusFLTKWindow::modifier()
{
  return (VisusModifier)((event_state() >> 16) & VISUS_MODIFIER_MASK);
}
 
void VisusFLTKWindow::keyboard(unsigned char key, int x, int y)
{
  switch (key) {

  case 'e':  
    if (mFocus != NULL) 
      popupMenu(mFocus);
    break;
  case 'b':
    cycleThroughBackgroundColors();
    break;
  default:
    VisusGuiBase::keyboard(key,x,y);
    break;
  }
  redisplay();
}


void VisusFLTKWindow::destroy()
{
  fltk::remove_idle(idle_callback,this);  
  fltk::GlWindow::destroy();
}


void VisusFLTKWindow::popupMenu(pVisusGroup focus)
{
  switch (focus->type()) {

  case VISUS_LABEL_NODE:
    if (mOpenMenu != NULL)
      delete mOpenMenu;
    mOpenMenu = new LabelNodeMenu(focus);
    break;
  case VISUS_ORTHO_SLICE:
    if (mOpenMenu != NULL)
      delete mOpenMenu;
    mOpenMenu = new OrthoSliceMenu(focus);
    break;
  case VISUS_TICK_MARKS:
    if (mOpenMenu != NULL)
      delete mOpenMenu;
    mOpenMenu = new TickMarksMenu(focus);
    break;
  default:
    vwarning("Fltk menu for node type %d not yet defined.",focus->type());
    break;
  }
}
    
    

    
