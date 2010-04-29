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


#ifndef VISUSSMARTPOINTER_H
#define VISUSSMARTPOINTER_H

#include <cstdlib>
#include "VisusAssert.h"
#include "VisusReadWriteLock.h"
#include "VisusAtomicCounter.h"
#include "VisusString.h"


void release_counter(VisusAtomicCounter<short>* counter);



/*! VisusSmartPointers implement a thread-safe reference counting
 *  scheme. The basic premise is the following: All objects that would
 *  like to be reference counted (most notably nodes and shared
 *  values) will derive from VisusManagedObject. Managed objects
 *  should \e only be created via the factory and no "normal" pointers
 *  to these classes should exists. Each time a new pointer is created
 *  the reference count in the object is increased each time a pointer
 *  is deleted the count is decreased. Once the count reaches 0 the
 *  object is destroyed. Note that this scheme requires some care by
 *  the user. One can always create new references from scratch using
 *  plain C pointers. This can easily make the reference counting
 *  inconsistent and thus should be used with care.In the following I
 *  will describe several pitfalls and things to keep in mind when
 *  implementing this basic scheme in a multi-thread environment.
 *
 *  Counters: One important aspect of reference counting is to
 *  remember that despite its appearance operator++ and operator-- are
 *  \e not atomic operations. If no mutex protection is provided
 *  special atomic counters (like VisusAtomicCounter) must be used to
 *  avoid loosing updates.
 * 
 *  Counter/Mutex placement: It is very tempting to put either an
 *  atomic counter and/or additional mutex protections into the
 *  managed object itself. This would allow a nicely contained
 *  implementation that could easily be maintained. However, this \e
 *  cannot work! Consider the following situation: Thread 1 owns the
 *  last pointer P0 to the object x. Now two things happen
 *  "simultaneously". Thread 1 destroys P0 and Thread 2 copies P0 into
 *  P1. If Thread 1 is faster it will decrease the counter and delete
 *  the object before Thread 2 has a chance to act. Now in Thread 2
 *  even the query whether the count is 0 is corrupted since the
 *  counter (and/or any mutexes that were part of it) no longer exist. 
 * 
 *  Counter/Mutex placement 2nd: There seem to be two straight forward
 *  solution to the previous problem. Either a) the counter is moved
 *  outside of the object or b) access to the counter is protected by
 *  a mechanism internal to the smart pointer (rather than internal to
 *  the managed object). a) This results in a chicken-and-egg problem
 *  since now someone must delete the counter at which point the old
 *  problems reappear. b) This would work in principle but it creates
 *  a serious bottle neck. Now every access to every pointer depends
 *  on the same mechnism which seems completely impractical.
 * 
 *  Solution 1 Global mutex: Rather than protecting things on the
 *  instance level (either in instances of the managed objects of
 *  instances of the pointers) one could use one/multiple global
 *  mutexes. The difficulty is in knowing which object is protected by
 *  which mutex and the granularity (how many global mutexes are
 *  necessarry to avoid contention). (Pitfall, the information which
 *  mutex belongs to which object must be static since otherwise it
 *  can become inconsistent
 *
 *  Solution 2 Timed counters: Using the cmpxchg operation one could
 *  implement a counter that once it reaches 0 can never increase
 *  again. Now we move all counters outside the managed objects
 *  (e.g. a global dynamic array) and include a timestamp when they
 *  were decreased to 0. Every once in a while one then performs a
 *  garbage collection deleting any 0-counter "older" than some
 *  threshold. This would allow perfect mutex-free per-object
 *  reference counting except that it is not guaranteed to work. In
 *  theory a thread could be stalled longer than the given threshold
 *  and thus still access an invalid counter. In practice object
 *  creation is fairly limited (how many nodes can you create/destroy)
 *  and thus a very large threshold (e.g. 10secs) could be choosen
 *  making it virtually certain that things work out. (Given the low
 *  memory footprint of counters one could also just "forget" to
 *  delete them...) (Pitfall, you now need to pass around both
 *  pointers to the object and pointers to its counter. Since these
 *  are two values they can become inconsistent.
 *
 *  Solution 3 Read write protect each single instance of a smart
 *  pointer. Only someone holding the lock can read/write the object
 *  pointer and/or modify the counter. This protects agains untimely
 *  copies. It seems that there still remains the problem of one
 *  thread trying to delete an object while another thread is trying
 *  to increase the counter.  However, the counter being 0 indicates
 *  that the destroying thread was the \e last reference around at the
 *  time of the function call. Any additional references are either
 *  created by copying this smart pointer or using an explicit
 *  construction. The local read/write lock guards against the first
 *  situation. The second situation cannot be guarded against and is
 *  left for the user to avoid.
 * 
 *  Solution 4: Combine 2) and 3). Move the counters to a global array
 *  such that they can never be accidentally deleted. Now protect all
 *  access to either counter or pointer within a smart pointer by a
 *  local mutex to ensure that both values are only read/modified as a
 *  pair. Finally incorporate the idea of a 0-sticky counter (a
 *  counter that does not increase once it has reached 0). As a new
 *  reference is created a 0-sticky counter tell you whether the
 *  object still exists (the counter was increased) or whether someone
 *  has (or is in the process of) deleted the object. 
 */
template<class BaseClass>
class VisusSmartPointer {
  
public:

  typedef BaseClass BaseClassType;  

  template <class T> friend class VisusSmartPointer; 
  
  friend class VisusFactory;
  friend class VisusGroup;

  //! Creting a NULL pointer
  explicit VisusSmartPointer();

  //! Copy constructor
  VisusSmartPointer(const VisusSmartPointer& pointer);

  //! Templatized Copy constructor
  template<class T>
  VisusSmartPointer(const VisusSmartPointer<T>& pointer);

  //! Destructor
  ~VisusSmartPointer();
  
  VisusSmartPointer& operator=(const VisusSmartPointer& pointer);

  template <class T>
  bool operator==(const VisusSmartPointer<T>& pointer) const {return (mPointer == pointer.mPointer);}
  bool operator==(const BaseClass* pointer) const {return (mPointer == pointer);}

  bool operator!=(const VisusSmartPointer& pointer) const {return (mPointer != pointer.mPointer);}
  bool operator!=(const BaseClass* pointer) const {return (mPointer != pointer);}

  BaseClass& operator*();
  const BaseClass& operator*() const;

  BaseClass* operator->();
  // Only Include If Not Being SWIG Parsed... If included this will only
  // provide python wrappings for const members
#ifndef SWIG
  const BaseClass* operator->() const;
#endif
  
  std::string getString() const;
  
  //! Casting to another smart pointer 
  //template <typename BaseCast>
  //operator VisusSmartPointer<BaseCast>() {return *this;}

  
  //! Const casting to another smart pointer
  //template <typename BaseCast>
  //operator VisusSmartPointer<BaseCast>() const {return (VisusSmartPointer<BaseCast>)*this;}
  
  bool isEmpty() const { return mPointer==NULL; }

private:

  //! Creating a new reference from scratch
  /*! Creating a new reference from scratch. This function should be
   *  used very sparingly and the caller must be able to \e guarantee
   *  that "object" will not be deleted during the construction of a
   *  new reference. Furthermore, the caller is responsible for
   *  increasing the reference count to the object
   */
  explicit VisusSmartPointer(BaseClass* object, VisusAtomicCounter<short>* counter);

  //! Pointer to the mananged object
  BaseClass* mPointer;
  
  //! Pointer to the counter protecting the object
  VisusAtomicCounter<short>* mCounter;

  //! The read/write lock
  VisusReadWriteLock mLock;
  
  int readLock() const {return mLock.readLock();}

  int writeLock() const {return mLock.writeLock();}

  int unlock() const {return mLock.unlock();}
};


template<class BaseClass>
std::string VisusSmartPointer<BaseClass>::getString() const
{
  std::ostringstream ss;
  ss << "VisusSmartPointer: counter(" << (mCounter!=NULL? mCounter->value():-1) 
	 << ") ptr(" << (mPointer!=NULL? toString<BaseClass>(mPointer).c_str() : "NULL") << ")";
  return ss.str();	
}

template<class BaseClass>
VisusSmartPointer<BaseClass>::VisusSmartPointer()
{
  mPointer = NULL;
  mCounter = NULL;

  vverbose("VSP<BC>::ctor() - out - %s\n", 1000, getString().c_str());
}


template<class BaseClass>
VisusSmartPointer<BaseClass>::VisusSmartPointer(BaseClass* object, VisusAtomicCounter<short>* counter) 
{
  vassert(object != NULL);
  vassert(counter != NULL);

  mPointer = object;
  mCounter = counter;
  mCounter->inc();

  vverbose("VSP<BC>::ctor(BC*,short*) - out - %s\n", 1000, getString().c_str());
}


template<class BaseClass>
VisusSmartPointer<BaseClass>::VisusSmartPointer(const VisusSmartPointer& pointer)
{
  vverbose("VSP<BC>::ctor(VSP<BC>) - in - %s\n", 1000,pointer.getString().c_str());
  
  if (pointer.readLock() == 0) {
    vwarning("Could not obtain read lock returning NULL pointer.");
    mPointer = NULL;
  }

  mPointer = pointer.mPointer;
  mCounter = pointer.mCounter;

  if ((mCounter != NULL) && (mCounter->constraintInc() == 0)) {
    mPointer = NULL;
    mCounter = NULL;
  }

  if (pointer.unlock() == 0) 
    vwarning("Could not unlock pointer.");

  vverbose("VSP<BC>::ctor(VSP<BC>) - out - %s\n", 1000,getString().c_str());
}

template<class BaseClass>
template<class T>
VisusSmartPointer<BaseClass>::VisusSmartPointer(const VisusSmartPointer<T>& pointer)
{
  vverbose("VSP<BC>::ctor(VSP<T>) - in - %s\n", 1000,pointer.getString().c_str());
  
  if (pointer.readLock() == 0) {
    vwarning("Could not obtain read lock returning NULL pointer.");
    mPointer = NULL;
  }

  mPointer = static_cast<BaseClass*>(pointer.mPointer);
  mCounter = pointer.mCounter;

  if ((mCounter != NULL) && (mCounter->constraintInc() == 0)) {
    mPointer = NULL;
    mCounter = NULL;
  }

  if (pointer.unlock() == 0) 
    vwarning("Could not unlock pointer.");

  vverbose("VSP<BC>::ctor(VSP<T>) - out - %s\n", 1000,getString().c_str());
}

template<class BaseClass>
VisusSmartPointer<BaseClass>::~VisusSmartPointer()
{

  if (writeLock() == 0) {
    vwarning("Could not obtain write lock in destructor."); 
    mPointer = NULL;
    return;
  }

  if (mCounter == NULL) {
    if (unlock() == 0) 
      vwarning("Could not unlock pointer.");
    return;
  }
 
  short count = mCounter->dec();
  
  vverbose("VSP<BC>::dtor() - in - %s\n", 1000,getString().c_str());
   
  if (count < 0) {
    vwarning("Reference count inconsistent ... object should be deleted by someone else.");
    mPointer = NULL;
    mCounter = NULL;
    if (unlock() == 0) 
      vwarning("Could not unlock pointer.");
    return;
  }    
  if (count == 0) {
	vverbose("VSP<BC>::dtor() - deleting object %s\n", 1000,getString().c_str());
    release_counter(mCounter);
    delete mPointer;

    mPointer = NULL;
    mCounter = NULL;
  }

  if (unlock() == 0) 
    vwarning("Could not unlock pointer.");
}

template<class BaseClass>
VisusSmartPointer<BaseClass>& VisusSmartPointer<BaseClass>::operator=(const VisusSmartPointer<BaseClass>& pointer)
{
  vverbose("VSP<BC>:: this(%s) = other(%s)\n", 1000,getString().c_str(), pointer.getString().c_str());

  short count;

  // If we have been assigned to ourselfs 
  if ((void*)&pointer == (void*)this)
    return *this; // There is nothing to do

  // First we must obtain a write-lock for myself
  if (writeLock() == 0) {
    vwarning("Could not obtain write-lock for pointer assignment. Assignment ignored.");
    return *this;
  }

  // Now also obtain a read-lock for pointer and handle the reference
  // count for the new reference
  if (pointer.readLock() == 0) {
    vwarning("Could not obtain read lock in pointer assignment. Assignment ignored.");
    if (unlock() == 0)
      vwarning("Could not unlock after pointer assignemnt.");

    return *this;
  }

  // If we need to change anything
  if (mPointer != pointer.mPointer) {

    // Handle the reference count for our old pointer
    if (mPointer != NULL) {
      vassert(mCounter != NULL);
      
      count = mCounter->dec();
      if (count < 0) 
        vwarning("Reference count inconsistent ... object should be deleted by someone else."); 
      if (count == 0) {
        delete mPointer;
        release_counter(mCounter);
      }
      
      mPointer = NULL;
      mCounter = NULL;
    }

    // Handle the reference count for the new pointer
    mPointer = pointer.mPointer;
    mCounter = pointer.mCounter;
    if ((mCounter != NULL) && (mCounter->constraintInc() == 0)) {
      mPointer = NULL;
      mCounter = NULL;
    }
  }    

  // Unlock pointer
  if (pointer.unlock() == 0)
    vwarning("Could not unlock pointer after assignment.");

  // Unlock myself
  if (unlock() == 0)
    vwarning("Could not unlock myself after assignment.");

  vverbose("VSP<BC>::= final %s\n", 1000,getString().c_str());

  return *this;
}



template<class BaseClass>
BaseClass& VisusSmartPointer<BaseClass>::operator*()
{
  if (mPointer == NULL) {
    vwarning("Dereferencing smart NULL pointer. Creating local object to avoid seg fault... this will leak memomry");
    return *(new BaseClass);
  }

  return *mPointer;
}

template<class BaseClass>
const BaseClass& VisusSmartPointer<BaseClass>::operator*() const
{
  if (mPointer == NULL) {
    vwarning("Dereferencing smart NULL pointer. Creating local object to avoid seg fault... this will leak memomry");
    return *(new BaseClass);
  }

  return *mPointer;
}

template<class BaseClass>
BaseClass* VisusSmartPointer<BaseClass>::operator->()
{
  if (mPointer == NULL) {
    vwarning("Dereferencing smart NULL pointer. Creating local object to avoid seg fault... this will leak memomry");
    return new BaseClass;
  }

  return mPointer;
}

// Only Include If Not Being SWIG Parsed...
#ifndef SWIG
template<class BaseClass>
const BaseClass* VisusSmartPointer<BaseClass>::operator->() const
{
  if (mPointer == NULL) {
    vwarning("Dereferencing smart NULL pointer. Creating local object to avoid seg fault... this will leak memomry");
    return new BaseClass;
  }

  return mPointer;
}
#endif






#endif
