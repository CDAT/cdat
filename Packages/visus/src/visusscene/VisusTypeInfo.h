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


#ifndef VISUSTYPEINFO_H
#define VISUSTYPEINFO_H

#include "VisusDefinitions.h"
#include "VisusFactory.h"
#include "VisusSmartPointer.h"
#include "VisusMutex.h"

/* Remember to add each new type to the gVisusSharedValueRepository()
   in the VisusSharedValue.cpp file and to the VisusFactor.cpp file
*/


#define VISUS_ISOVALUE_TYPEID         0
#define VISUS_BOUNDINGBOX_TYPEID      1
#define VISUS_FONT_TYPEID             2
#define VISUS_TRANSFORMATION2D_TYPEID 3
#define VISUS_TRANSFORMATION3D_TYPEID 4
#define VISUS_DATAREQUEST_TYPEID      5
#define VISUS_DATADESCRIPTION_TYPEID  6
#define VISUS_FIELDINDEX_TYPEID       7
#define VISUS_COLORMAP_TYPEID         8
#define VISUS_COLOR_TYPEID            9
#define VISUS_CAMERA_TYPEID          10
#define VISUS_EARTHRADIUS_TYPEID     11
#define VISUS_GLOBALTIME_TYPEID      12
#define VISUS_OPENGLSTATE_TYPEID     13


#define VISUS_NUM_TYPEIDS            14

#ifndef SWIG

/*
  int getValue(BaseName& value) const {value = mData;return 1;}         \
  int getValue(BaseName* value) const {*value = mData;return 1;}        \
                                                                        \
  int setValue(const BaseName& value) {mData = value;return 1;}         \
  int setValue(const BaseName* value) {mData = *value; return 1;}
*/

#define VISUS_DECLARE_ATOMIC(ClassName,BaseName)                        \
  public:                                                               \
                                                                        \
  typedef BaseName VisusValueClass;                                     \
                                                                        \
  static const char* sTypeName;                                         \
  static const int sTypeIndex;                                          \
                                                                        \
  virtual pVisusSharedValue clone() const {                             \
    return gObjectFactory.constructSharedValue<ClassName>(*this);       \
  }                                                                     \
  virtual pVisusSharedValue instantiate() const {                       \
    return gObjectFactory.constructSharedValue<ClassName>();            \
  }                                                                     \
  virtual const char* getName() const {return sTypeName;}               \
  virtual int getIndex() const {return sTypeIndex;}                     \
                                                                        \
  static int lockToReadStatic() {return 1;}                             \
  static int unlockAfterReadStatic() {return 1;}                        \
  static int lockToWriteStatic() {return 1;}                            \
  static int unlockAfterWriteStatic() {return 1;}                       \
                                                                        \
                                                                        \
  void getValue(BaseName& buffer) const {                               \
    buffer = *this;                                                     \
  }                                                                     \
                                                                        \
  void setValue(const BaseName& buffer) {                               \
    *static_cast<BaseName*>(this) = buffer;                             \
  }                                                                     

#define VISUS_DECLARE_COMBINED(ClassName,BaseName)                      \
  public:                                                               \
                                                                        \
  typedef BaseName VisusValueClass;                                     \
                                                                        \
  static const char* sTypeName;                                         \
  static const int sTypeIndex;                                          \
                                                                        \
  virtual pVisusSharedValue clone() const {                             \
    return gObjectFactory.constructSharedValue<ClassName>(*this);       \
  }                                                                     \
  virtual pVisusSharedValue instantiate() const {                       \
    return gObjectFactory.constructSharedValue<ClassName>();            \
  }                                                                     \
  virtual const char* getName() const {return sTypeName;}               \
  virtual int getIndex() const {return sTypeIndex;}                     \
                                                                        \
  static VisusMutex sSharedValueMutex;                                  \
  static VisusAtomicCounter<short> sAccessCount;                        \
  static int lockToReadStatic()  {                                      \
    if (sSharedValueMutex.lock() == 0) {                                \
      vwarning("Could not obtain lock for reading");                    \
      return 0;                                                         \
    }                                                                   \
    sAccessCount++;                                                     \
    if (sSharedValueMutex.unlock() == 0) {                              \
      vwarning("Could not unlock after increasing access counter");     \
      return 0;                                                         \
    }                                                                   \
    return 1;                                                           \
  }                                                                     \
  static int unlockAfterReadStatic()  {                                 \
    sAccessCount--;                                                     \
    if (sAccessCount < 0) {                                             \
      vwarning("SharedValue access count inconsistent");                \
      sAccessCount = 0;                                                 \
      return 0;                                                         \
    }                                                                   \
    return 1;                                                           \
  }                                                                     \
  static int lockToWriteStatic() {                                      \
    if (sSharedValueMutex.lock() == 0) {                                \
      vwarning("Could not obtain lock for writing");                    \
      return 0;                                                         \
    }                                                                   \
    while (sAccessCount > 0) {                                          \
      sleepmillisec(VISUS_MUTEX_WAIT);                                  \
    }                                                                   \
    return 1;                                                           \
  }                                                                     \
                                                                        \
  static int unlockAfterWriteStatic() {                                 \
    if (sSharedValueMutex.unlock() == 0) {                              \
      vwarning("Could not unlock after writing");                       \
      return 0;                                                         \
    }                                                                   \
    return 1;                                                           \
  }                                                                     \
                                                                        \
  void getValue(BaseName& buffer) const {                               \
    buffer = *this;                                                     \
  }                                                                     \
                                                                        \
  void setValue(const BaseName& buffer) {                               \
    *static_cast<BaseName*>(this) = buffer;                             \
  }                                                                     \
                                                                        \
  void toXML(XMLNode& parent) const {                                   \
    BaseName::toXML(parent);                                            \
  }                                                                     \
                                                                        \
  bool fromXML(XMLNode& node)       {                                   \
    return BaseName::fromXML(node);                                     \
  }                                                                     


/*                                                                      \
  int lockToRead() const {return ClassName::lockToReadStatic();}        \
  int unlockAfterRead() const  {return ClassName::unlockAfterReadStatic();} \
  int lockToWrite() const {return ClassName::lockToWriteStatic();}      \
  int unlockAfterWrite() const {return ClassName::unlockAfterWriteStatic();}                     
*/
/*
  int getValue(BaseName& value) const {return getValueInternal<BaseName>(value);} \
  int getValue(BaseName* value) const {return getValueInternal<BaseName>(*value);} \
                                                                                   \
  int setValue(const BaseName& value) {return setValueInternal<BaseName>(value);}  \
  int setValue(const BaseName* value) {return setValueInternal<BaseName>(*value);}
*/
  

#endif

#endif
