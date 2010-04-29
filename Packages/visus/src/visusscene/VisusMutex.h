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


#ifndef VISUSMUTEX_H
#define VISUSMUTEX_H

#include "VisusAssert.h"

#ifdef VISUS_NO_PTHREAD

class VisusMutex
{
public:
  VisusMutex() {vwarning("Visus compiled with VISUS_NO_PTHREAD cannot use VisusMutex");}
  VisusMutex(const VisusMutex& mutex) {vwarning("Visus compiled with VISUS_NO_PTHREAD cannot use VisusMutex");}
  VisusMutex& operator=(const VisusMutex& mutex) {return *this;}
  ~VisusMutex() {vwarning("Visus compiled with VISUS_NO_PTHREAD cannot use VisusMutex");}
  int tryLock() {vwarning("Visus compiled with VISUS_NO_PTHREAD cannot use VisusMutex"); return 1;}
  int lock() {vwarning("Visus compiled with VISUS_NO_PTHREAD cannot use VisusMutex"); return 1;}
  int unlock() {vwarning("Visus compiled with VISUS_NO_PTHREAD cannot use VisusMutex");return 1;}
};

/* Deprecated see below
class VisusMutexPointer
{
public:


  VisusMutexPointer() {}
  VisusMutexPointer(VisusMutex &mutex) {vwarning("Visus compiled with VISUS_NO_PTHREAD cannot use VisusMutex");}
  VisusMutexPointer(const VisusMutexPointer& mutex)  {vwarning("Visus compiled with VISUS_NO_PTHREAD cannot use VisusMutex");}
  ~VisusMutexPointer() {}

  VisusMutexPointer& operator=(const VisusMutexPointer& mutex) {return *this;}
  VisusMutex& operator*() {return mMutex;}
  VisusMutex* operator->() {return NULL;}

  bool valid() const {return false;}

  void invalidate() {}
private:
  VisusMutex mMutex;
};
*/

#else

#include <pthread.h>


//! Wrapper around a pthread mutex
/*! This class forms a wrapper around a pthread mutex. The wrapper
 *  serves two purposes: First, it separates the mutex implementation
 *  from the rest of the system. This allows a simple
 *  re-implementation of the mutex using a different library. Second,
 *  the VisusMutex implements a reference counting to keep track of
 *  all VisusMutexPointer instance around. The reference counting is
 *  used both for debugging as well as to allow thread-safe
 *  destruction of mutexes.
 */
class VisusMutex
{
public:
  
  friend class VisusMutexPointer;

  VisusMutex();
  ~VisusMutex();

  bool operator==(const VisusMutex& mutex) {return mMutex == mutex.mMutex;}
  bool operator!=(const VisusMutex& mutex) {return mMutex != mutex.mMutex;}

  int tryLock();

  int lock();

  int unlock();

private:

  pthread_mutex_t* mMutex;
};


//! Wrapper around the pointer to a VisusMutex
/*! This class implements a managed pointer to a VisusMutex. It
 *  overloads all standard pointer-like access function but implements
 *  a reference counting scheme to keep track of how many pointer
 *  reference a given VisusMutex.
 */
/* VisusMutexPointer is deprecated. It was designed to be used with
 * the data pipe construct which is no longer used. However, the mutex
 * pointers are the only reason a Mutex contains a counter which
 * creates a circular reference with VisusAtomicCounter when no
 * hardware counters are used. Therefore, I removed the MutexPointers
 * copmletely (06/05/08) ptb

class VisusMutexPointer
{
public:

  VisusMutexPointer();
  VisusMutexPointer(VisusMutex &mutex);
  VisusMutexPointer(const VisusMutexPointer& mutex);

  ~VisusMutexPointer();

  VisusMutexPointer& operator=(const VisusMutexPointer& mutex);
  VisusMutex& operator*() {return *mMutex;}
  VisusMutex* operator->() {return mMutex;}

  bool valid() const {return (mMutex != NULL);}

  void invalidate();
private:

  VisusMutex* mMutex; 
};
*/

#endif

#endif
