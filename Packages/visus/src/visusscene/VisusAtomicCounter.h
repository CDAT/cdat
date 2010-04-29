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


#ifndef VISUSATOMICCOUNTER_H
#define VISUSATOMICCOUNTER_H

/*! \file This file contains the various implementations of an \e
 *  atomic counter. An atomic counter should behave just like a normal
 *  integer counter except that is not subject to \e
 *  race-conditions. Despite the fact that, for example, a
 *  operator++(int) call can be (and likly \e is) implemented using a
 *  single assembler call (inc in this case) it is still at its core a
 *  \e read-modify-write call. As a result it is subject to
 *  race-conditions and thus cannot be safely shared among threads. An
 *  atomic acounter handles this problem by protecting the counter
 *  against concurrent accesses. This can be done in two ways: a)
 *  using low level assembler code using the \e lock flag (at least on
 *  X86 type machines) or b) using mutexes. This first variant is
 *  expected to be orders of magnitueds faster than the second and
 *  therefore is used as default. To enable the pthread-mutex version
 *  the code can be compiled with -DVISUS_PROTECT_ATOMIC
 */

//#define VISUS_PROTECT_ATOMIC 1

#ifndef VISUS_DISABLE_HARDWARE_COUNTER


namespace atomicCounter {

#ifdef WIN32
  //! Increase a four byte counter by one
  void __fastcall Inc (volatile int*);  // 4 bytes

  //! Increase a two byte counter by one
  void __fastcall Inc (volatile short*);  // 2 bytes

  //! Increase a byte-sized counter by one
  void __fastcall Inc (volatile char*);  // 1 byte

  //! Decrease a four byte counter by one
  int __fastcall Dec (volatile int*);  // 4 bytes

  //! Decrease a two byte counter by one
  short __fastcall Dec (volatile short*);  // 2 bytes

  //! Decrease a byte-sized counter by one
  char __fastcall Dec (volatile char*);  // 1 byte

  int  __fastcall CompareAndSwap (volatile int*  dest,  int source,   int comparend);
  short __fastcall CompareAndSwap (volatile short* dest, short source, short comparend);
  char __fastcall CompareAndSwap (volatile char*  dest, char source,  char comparend);
#else

  
  void  Inc (volatile int*);  // 4 bytes
  void  Inc (volatile short*);  // 2 bytes
  void  Inc (volatile char*);  // 1 byte

  int   Dec (volatile int*);  // 4 bytes
  short Dec (volatile short*);  // 2 bytes
  char  Dec (volatile char*);  // 1 byte

  int   CompareAndSwap (volatile int* dest, int source, int comparend);
  short CompareAndSwap (volatile short* dest, int source, int comparend);
  char  CompareAndSwap (volatile char* dest, int source, int comparend);
#endif

}


/*! An atomic counter implements a one-, two-, or four-byte signed or
 *  unsigned counter that is not subject to race conditions. The
 *  interface is purposefully kept sparse to allow an easy
 *  implementation using different techniques (assembly calls,
 *  software mutexes). In particular, the increment/decrement
 *  functions do not immediately return the current count to prevent
 *  the counter from being abused.
 */
template <typename T>
class VisusAtomicCounter {

public:
  
  //! Default constructor that \e initializes the counter to 0
  VisusAtomicCounter() : mValue(0) {}

  //! Copy constructor
  VisusAtomicCounter(T value) : mValue(value) {}

  //! Type conversion to the underlying data type
  operator T() const volatile { return mValue;}

  //! Comparison with a value of the base type
  bool operator==(T value) const volatile {return (value == mValue);}

  //! Comparison with a value of the base type
  bool operator!=(T value) const volatile {return (value != mValue);}

  //! Return the current value
  T value() {return mValue;}

  //! Increases the counter by one 
  void inc() volatile {atomicCounter::Inc(&mValue);}

  //! Increase the counter by one unless it has reach 0 
  T constraintInc() volatile;

  //! Decreases the counter by one
  T dec() volatile {return atomicCounter::Dec(&mValue);}

  //! Postfix-increase with \e no return value 
  void operator++(int) volatile {inc();}

  //! Postfix-decrease with \e no return value
  void operator--(int) volatile {dec();}

protected:

  //! Member variable storing the current count
  T mValue;
};


/*! This is a somewhat tricky template to perform an atomic
 *  compare-and-increase operation. The goal is to increase the
 *  counter by 1 \e unless the counter reached 0 at any point in
 *  time. We use the CompareAndSwap(counter,new,old) function which
 *  compares counter to old and either switches counter to new if
 *  old==counter or leaves counter unchanged otherwise. It always
 *  returns old.
 */
template <typename T>
T VisusAtomicCounter<T>::constraintInc() volatile
{
  T c,old;
  c = mValue; // Get the "current" value and we don't particularly
              // care if someone changes it after we read

  // The while loop has two exit conditions a) c = 0: in this case the
  // first && is false which is != c(=0) and thus the loop stops; or
  // b) c>0 and old==c. In this case the first && is true and != c(>0)
  // and the loops stops. However, if old=c the Compare and Swap must
  // have performed an increment so the current value of the counter
  // (as seen by this function) is c+1
  while (c && (old = atomicCounter::CompareAndSwap(&mValue,c+1,c)) != c)
    c = old;

  return c+1;
}



#else 

#include "VisusMutex.h"

template <typename T>
class VisusAtomicCounter {

public:

  VisusAtomicCounter(T value) : mValue(value) {}
  VisusAtomicCounter() : mValue(0) {}

  operator T() const volatile { return mValue;}
  bool operator==(T value) const volatile {return (value == mValue);}
  bool operator!=(T value) const volatile {return (value != mValue);}

  T constraintInc();

  //! Return the current value
  T value() {return mValue;}

  T inc() { mMutex.lock();++mValue;mMutex.unlock();return mValue;}
  T dec() { mMutex.lock();--mValue;mMutex.unlock();return mValue;}

  //T inc() volatile { return ++mValue;}
  //T dec() volatile { return --mValue;}

  T operator++(int) {return inc();}

  T operator--(int) {return dec();}

protected:
  T mValue;

  VisusMutex mMutex;
};

template <typename T>
T VisusAtomicCounter<T>::constraintInc()
{
  mMutex.lock();
  
  if (mValue > 0)
    ++mValue;
  
  mMutex.unlock();
  return mValue;
}

#endif

#endif
