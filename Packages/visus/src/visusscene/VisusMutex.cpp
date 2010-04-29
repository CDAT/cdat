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


#ifndef VISUS_NO_PTHREAD

#include <cstdio>
#include "VisusMutex.h"

VisusMutex::VisusMutex()
{
  mMutex = new pthread_mutex_t;
  
  if (pthread_mutex_init(mMutex,NULL) != 0) {
 
   vwarning("Mutex could not be initialized ... invalidating mutex");
   delete mMutex;
   mMutex = NULL;   
  }
}

VisusMutex::~VisusMutex()
{
  //vwarning("VisusMutex destructor");

  if (pthread_mutex_destroy(mMutex) != 0)     
   vwarning("Mutex could not be destroyed ... proceeding at your own risk");

  delete mMutex;
}

int VisusMutex::tryLock()
{
  if (mMutex == NULL) {
    vwarning("Accessing invalid Mutex .. ignoring lock");
    return 0;
  }

  if (pthread_mutex_trylock(mMutex) != 0) 
    return 0;
  else
    return 1;
}

int VisusMutex::lock()
{
  //fprintf(stderr,"VisusMutex::lock  start\n");

  if (mMutex == NULL) {
    vwarning("Accessing invalid Mutex .. ignoring lock");
    return 0;
  }

  if (pthread_mutex_lock(mMutex) != 0) {
    vwarning("Mutex could not be locked. Expect invalid threading behavior");
    return 0;
  }

  //fprintf(stderr,"VisusMutex::lock  end\n");
  return 1;
}

int VisusMutex::unlock()
{
  //fprintf(stderr,"VisusMutex::unlock  start\n");
  if (mMutex == NULL) {
    vwarning("Accessing invalid Mutex .. ignoring unlock");
    return 0;
  }

  if (pthread_mutex_unlock(mMutex) != 0) {
    vwarning("Mutex could not be unlocked. Expect invalid threading behavior");
    return 0;
  }

  //fprintf(stderr,"VisusMutex::unlock  end\n");
  return 1;
}


/* 
VisusMutexPointer::VisusMutexPointer()
{
  mMutex = NULL;
}


VisusMutexPointer::VisusMutexPointer(VisusMutex& mutex)
{
  mMutex = &mutex;
  mMutex->increaseRefCount();
}


VisusMutexPointer::VisusMutexPointer(const VisusMutexPointer& mutex) : mMutex(mutex.mMutex) 
{
  if (mutex.mMutex == NULL) {
    mMutex = NULL;
  }
  else {
    mMutex = mutex.mMutex;
    mMutex->increaseRefCount();
  }
}

VisusMutexPointer::~VisusMutexPointer()
{
  if (mMutex != NULL)
    mMutex->decreaseRefCount();
}
 
VisusMutexPointer& VisusMutexPointer::operator=(const VisusMutexPointer& mutex)
{
  if (mutex.mMutex == NULL)
    mMutex = NULL;
  else {

    if (mMutex != mutex.mMutex) {
      if (mMutex != NULL)
        mMutex->decreaseRefCount();
      
      mMutex = mutex.mMutex;
      mMutex->increaseRefCount();
    }
  }

  return *this;
}
 
void VisusMutexPointer::invalidate()
{
  if (mMutex != NULL)
    mMutex->decreaseRefCount();

  mMutex = NULL;
}
*/

#endif
