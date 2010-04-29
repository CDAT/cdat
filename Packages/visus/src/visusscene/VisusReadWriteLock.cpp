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


#include "VisusReadWriteLock.h"
#include <sys/types.h>
#include <cstdio>

#ifndef VISUS_DISABLE_PTHREADS

VisusReadWriteLock::VisusReadWriteLock():
	mInit(false)
{
  if (pthread_rwlock_init(&mLock,NULL) != 0) {    
    vwarning("Read'Write lock could not be initialized ... invalid mutex.");
  }
  else {
    mInit = true;
  }
}


VisusReadWriteLock::~VisusReadWriteLock()
{
  if (mInit) {
    if (pthread_rwlock_destroy(&mLock) != 0) 
      vwarning("Read/Write lock could not be destroyed.");
  }
}


int VisusReadWriteLock::readLock() const
{
  if (! mInit) {
    vwarning("Accessing invalid read/write lock .. ignoring lock.");
    return 0;
  }

  if (pthread_rwlock_rdlock(const_cast <pthread_rwlock_t*>(&mLock)) != 0) {
    vwarning("Could not obtain read lock.");
    return 0;
  }

  return 1;
}

int VisusReadWriteLock::writeLock() const 
{
  if (! mInit) {
    vwarning("Accessing invalid read/write lock .. ignoring lock.");
    return 0;
  }

  int err;
  if ((err = pthread_rwlock_wrlock(const_cast <pthread_rwlock_t*>(&mLock))) != 0) {
    vwarning("Could not obtain write lock.");
    fprintf(stderr,"%d \n",err);
    return 0;
  }

  return 1;
}

int VisusReadWriteLock::unlock() const
{
  if (! mInit) {
    vwarning("Accessing invalid read/write lock .. ignoring lock.");
    return 0;
  }

  if (pthread_rwlock_unlock(const_cast <pthread_rwlock_t*>(&mLock)) != 0) {
    vwarning("Could not unlock read/write lock.");
    return 0;
  }

  return 1;
}

#endif
