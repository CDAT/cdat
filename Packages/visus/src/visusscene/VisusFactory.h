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


#ifndef VISUSFACTORY_H
#define VISUSFACTORY_H

#include <list>

#include "VisusAtomicCounter.h"
#include "VisusSmartPointer.h"

#include "VisusNodeTypes.h"

//enum VisusNodeType;
class VisusGroup;
class VisusSharedValue;

class VisusFactory
{
public:

  struct GarbageItem {    
    VisusAtomicCounter<short>* counter;
    time_t timeStamp;
  };

  VisusFactory() {}
  
  virtual ~VisusFactory();

  void releaseCounter(VisusAtomicCounter<short>* counter);
  
  VisusSmartPointer<VisusGroup> createNode(VisusNodeType nodeType);

  template<class ManagedClass>
  VisusSmartPointer<VisusGroup> constructNode();

  template<class ManagedClass>  
  VisusSmartPointer<VisusGroup> createNodeReference(ManagedClass* node);

  template<class ManagedClass>
  VisusSmartPointer<VisusGroup> constructNode(const ManagedClass& n);

  template<class ManagedClass>
  VisusSmartPointer<VisusSharedValue> constructSharedValue();

  VisusSmartPointer<VisusSharedValue> constructSharedValue(int type_index);

  template<class ManagedClass>
  VisusSmartPointer<VisusSharedValue>  createSharedValueReference(ManagedClass* value);

  template<class ManagedClass>
  VisusSmartPointer<VisusSharedValue> constructSharedValue(const ManagedClass& v);  
  
  template<class ManagedClass>
  VisusSmartPointer<ManagedClass> instantiate();

private:
  
  std::list<GarbageItem> mGarbage;
};
  
extern VisusFactory gObjectFactory; 


template<class ManagedClass>
VisusSmartPointer<VisusGroup> VisusFactory::constructNode()
{
  ManagedClass* node;
  VisusAtomicCounter<short>* counter;

  counter = new VisusAtomicCounter<short>();
  node = new ManagedClass();
  node->referenceCount(counter);

  //pointer = VisusSmartPointer<VisusGroup>(node,counter);

  return VisusSmartPointer<VisusGroup>(node,counter);
  //return pointer;
}

template<class ManagedClass>
VisusSmartPointer<VisusGroup> VisusFactory::createNodeReference(ManagedClass* node)
{
  // If there is no such node return the NULL pointer
  if (node == NULL) {
    vwarning("Cannot construct node from NULL.");
    return VisusSmartPointer<VisusGroup>();
  }
  
  VisusSmartPointer<VisusGroup> pointer;

  // If we could not increase the counter the node will be delete
  // shortly and we return NULL to avoid trouble
  if (node->referenceCount()->constraintInc() == 0)  
    pointer = VisusSmartPointer<VisusGroup>();
  else // otherwise construct a new smart pointer
    pointer = VisusSmartPointer<VisusGroup>(node,node->referenceCount());

  return pointer;
}

template<class ManagedClass>
VisusSmartPointer<VisusGroup> VisusFactory::constructNode(const ManagedClass& n)
{
  ManagedClass* node;
  VisusAtomicCounter<short>* counter;

  counter = new VisusAtomicCounter<short>(); // VSP:ctor inc's
  node = new ManagedClass(n);
  node->referenceCount(counter);

  return VisusSmartPointer<VisusGroup>(node,counter);
}

template<class ManagedClass>
VisusSmartPointer<VisusSharedValue> VisusFactory::constructSharedValue()
{
  ManagedClass* value;
  VisusAtomicCounter<short>* counter;

  counter = new VisusAtomicCounter<short>(); // VSP:ctor inc's
  value = new ManagedClass();
  value->referenceCount(counter);

  return VisusSmartPointer<VisusSharedValue>(value,counter);
}

template<class ManagedClass>
VisusSmartPointer<VisusSharedValue> VisusFactory::createSharedValueReference(ManagedClass* value)
{
  if (value == NULL) {
    vwarning("Cannot construct shared value from NULL.");
    return VisusSmartPointer<VisusSharedValue>();
  }

  // If we could not increase the counter the node will be delete
  // shortly and we return NULL to avoid trouble
  if (value->referenceCount()->constraintInc() == 0)  
    return VisusSmartPointer<VisusSharedValue>();
  else // otherwise construct a new smart pointer
    return VisusSmartPointer<VisusSharedValue>(value,value->referenceCount());
}

template<class ManagedClass>
VisusSmartPointer<VisusSharedValue> VisusFactory::constructSharedValue(const ManagedClass& v)
{
  ManagedClass* value;
  VisusAtomicCounter<short>* counter;

  counter = new VisusAtomicCounter<short>(); // VSP:ctor inc's
  value = new ManagedClass(v);
  value->referenceCount(counter);

  return VisusSmartPointer<VisusSharedValue>(value,counter);
}

template<class ManagedClass>
VisusSmartPointer<ManagedClass> VisusFactory::instantiate()
{
  ManagedClass* node;
  VisusAtomicCounter<short>* counter;
  
  //fprintf(stderr,"Instantiating class\n");

  counter = new VisusAtomicCounter<short>();
  node = new ManagedClass();
  node->referenceCount(counter);

  return VisusSmartPointer<ManagedClass>(node,counter);
}

#endif
