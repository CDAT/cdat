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


#include "VisusFLTKGui.h"
#include "VisusFLTKWindow.h"
#include "fltk/run.h"
#include <algorithm>
#include <iostream>

const float VisusFLTKGui::sUpdateInterval = 0.1;

VisusFLTKGui::VisusFLTKGui()
{
}


VisusFLTKGui::~VisusFLTKGui()
{
  std::vector<VisusFLTKWindow*>::iterator it;

  for (it=mTrash.begin();it!=mTrash.end();it++) {
    // just in case the windows aren't destroyed yet we destroy them
    // now. Note that a double destroy should have no ill effects
    (*it)->destroy();

    delete *it;
  }
}


int VisusFLTKGui::createWindow(pVisusGroup root)
{
  if (root == NULL) {
    vwarning("Cannot create window with empty scene graph.");
    return 0;
  }

  //fprintf(stderr,"VisusFLTKGui::createWindow\n");
  
  VisusFLTKWindow* window;
  
  char label[20] = "Visus2.0";

  if (mActiveWindows.size() > 0) 
    sprintf(label,"Visus2.0 -%d-",(int)mActiveWindows.size());  

  window = new VisusFLTKWindow(label);

  window->setRoot(root);
  window->show();
  mActiveWindows.push_back(window);

  return 0;
}

int VisusFLTKGui::mainLoop()
{
  return fltk::run();
}

const VisusFLTKWindow& VisusFLTKGui::window(int i)
{
  if ((i < 0) || (i >= (int)mActiveWindows.size())) {
    vwarning("Index out of range no such window.");
    return NULL;
  }

  return *mActiveWindows[i];
}


int VisusFLTKGui::update()
{
  //fprintf(stderr,"VisusFLTKGui::mainLoop\n");

  fltk::wait(sUpdateInterval);

  std::vector<VisusFLTKWindow*>::iterator it;
  it = mActiveWindows.begin();

  while (it != mActiveWindows.end()) {
    if (!(*it)->shown()) {
      std::iter_swap(it,mActiveWindows.rbegin());
      mTrash.push_back(mActiveWindows.back());
      mActiveWindows.pop_back();

      //mTrash.back()->destroy();
      delete mTrash.back();

      mTrash.pop_back();
      fprintf(stderr,"Deactivated window\n");
    }
    else 
      it++;
  }
   
  if (mActiveWindows.empty())
    return 0;
  else
    return 1;
}
