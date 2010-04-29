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

#ifndef VISUSGUIINTERFACE_H
#define VISUSGUIINTERFACE_H

#include <queue>

#include "VisusStdInt.h"
#include "VisusMutex.h"
#include "VisusGroup.h"

enum VisusMessageID {
  
  VISUS_CREATE_WINDOW    = 0,
  VISUS_DESTROY_WINDOW   = 1,
  VISUS_DESTROY_INACTIVE = 2,
};

//! A message structure
struct VisusMessage {
  
  VisusMessageID id;
  int win_id;
  void *buffer;
};


//! Interface to be implemented for each user interface
class VisusGuiInterface 
{
public:

  //! Constructor 
  VisusGuiInterface() {}

  //! Destructor
  virtual ~VisusGuiInterface() {}
 
  //! Create a window
  virtual int createWindow(pVisusGroup root);

  //! Destroy the gui if all windows are inactive
  virtual int destroyInactive();

  //! Post a message to the i'th window
  virtual int post(VisusMessage msg);

  //! The function calling the mainloop
  virtual void mainloop() = 0;

protected:

  //! Internal message queue
  std::queue<VisusMessage> mMessageQueue;

  //! Mutex protecting the message queue
  VisusMutex mMutex;
};

#endif
