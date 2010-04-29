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


#include <cstring>
#include <algorithm>

#include "VisusSharedValue.h"
#include "VisusAssert.h"
#include "VisusSharedIsoValue.h"
#include "VisusSharedBoundingBox.h"
#include "VisusSharedFont.h"
#include "VisusSharedTransformation2D.h"
#include "VisusSharedTransformation3D.h"
#include "VisusSharedDataRequest.h"
#include "VisusSharedDataDescription.h"
#include "VisusSharedFieldIndex.h"
#include "VisusSharedColorMap.h"
#include "VisusSharedColor.h"
#include "VisusSharedCamera.h"
#include "VisusSharedEarthRadius.h"
#include "VisusSharedGlobalTime.h"
#include "VisusSharedOpenGLState.h"


//! Global array storing one instance of each possible shared value
/*! The shared value repository is a global array which stores one
 *  instance of each possible shared value class. The array is used to
 *  avoid constant switch statements when the type is only know by its
 *  type index. For example, when locking a shared value one often
 *  cannot relie on having an object at hand and locking-by-index
 *  would require a switch statement to access the appropriate static
 *  function. Note, that static functions \b cannot be declared
 *  virtual and thus this seems to be the only way handeling the
 *  problem. There are two reasons why avoinding the switch statement
 *  seems a good idea. First, in the furture there might well be a
 *  serious performance penalty involved with calling a large switch
 *  statement each time a shared value is accessed. Second, there are
 *  already at least five different functions that would use a switch
 *  statement and each time a new shared value class is added \e all
 *  of them would need to be updated. This is error prone an puts an
 *  annoying burden on the programmer.
 *
 *  The reason why the array is initialized in this way is to avoid
 *  the static-initialization-order-fiasco. The implementation follows
 *  the construct-on-first-use idiom. Since the local variable
 *  sSharedValueRepo is declared static the initialization will be
 *  performed only once. However, it is guaranteed to be performed
 *  before it is ever used which solves our issue. Note, that we pay a
 *  price in that the global array in principle will leak
 *  memory. However, it is constructed only once and the OS will
 *  reclaim any memory on exit of any program so unless important work
 *  is to be performed in the destructor calls (e.g. file output) this
 *  seems of no concern. 
 * 
 *  For more information <href
 *  http://www.parashift.com/c++-faq-lite/ctors.html#faq-10.14> this
 *  </href> is a good place to start.
 *
 *  If in the furtur there appear problems with this method it can
 *  always be replaced by an initialization function that any user
 *  would be required to call first thing in main, during import of a
 *  python module etc..
 */
pVisusSharedValue* gVisusSharedValueRepository()
{
  static pVisusSharedValue sSharedValueRepo[VISUS_NUM_TYPEIDS] = {
    gObjectFactory.constructSharedValue<VisusSharedIsoValue>(),
    gObjectFactory.constructSharedValue<VisusSharedBoundingBox>(),
    gObjectFactory.constructSharedValue<VisusSharedFont>(),
    gObjectFactory.constructSharedValue<VisusSharedTransformation2D>(),
    gObjectFactory.constructSharedValue<VisusSharedTransformation3D>(),
    gObjectFactory.constructSharedValue<VisusSharedDataRequest>(),
    gObjectFactory.constructSharedValue<VisusSharedDataDescription>(),
    gObjectFactory.constructSharedValue<VisusSharedFieldIndex>(),
    gObjectFactory.constructSharedValue<VisusSharedColorMap>(),
    gObjectFactory.constructSharedValue<VisusSharedColor>(),
    gObjectFactory.constructSharedValue<VisusSharedCamera>(),
    gObjectFactory.constructSharedValue<VisusSharedEarthRadius>(),
    gObjectFactory.constructSharedValue<VisusSharedGlobalTime>(),
    gObjectFactory.constructSharedValue<VisusSharedOpenGLState>(),
  };

  return sSharedValueRepo;
}


pVisusSharedValue VisusSharedValue::instantiate(int type_index) 
{
  type_index_check(type_index,pVisusSharedValue());

  return gVisusSharedValueRepository()[type_index]->instantiate();

  /*
  VisusSharedValue *value = NULL;

  switch (type_index) {

  case 0:
    value =  new VisusSharedIsoValue();
    break;
  case 1:
    value =  new VisusSharedBoundingBox();
    break;
  case 2:
    value =  new VisusSharedFont();
    break;
  case 3:
    value =  new VisusSharedTransformation2D();
    break;
  case 4:
    value =  new VisusSharedTransformation3D();
    break;
  default:
    vwarning("SharedValue::instantiate type_index exists but but is not bein instantiated.\
 Update switch statement. No value returned");
    return NULL;
    break;
  }

  if (value->getIndex() != type_index) {
    vwarning("SharedValueFactory inconsistent incorrrect type no value returned");
    delete value;
    return NULL;
  }

  return value;
  */
}
const pVisusSharedValue VisusSharedValue::dummySharedValue(int type_index)
{
  type_index_check(type_index,pVisusSharedValue());

  return gVisusSharedValueRepository()[type_index];
}

int VisusSharedValue::lockToRead(int type_index)
{
  type_index_check(type_index,0);

  return gVisusSharedValueRepository()[type_index]->lockToRead();
}

int VisusSharedValue::unlockAfterRead(int type_index)
{
  type_index_check(type_index,0);

  return gVisusSharedValueRepository()[type_index]->unlockAfterRead();
}

int VisusSharedValue::lockToWrite(int type_index)
{
  type_index_check(type_index,0);

  return gVisusSharedValueRepository()[type_index]->lockToWrite();
}

int VisusSharedValue::unlockAfterWrite(int type_index)
{
  type_index_check(type_index,0);

  return gVisusSharedValueRepository()[type_index]->unlockAfterWrite();
}

pVisusSharedValue VisusSharedValue::clone() const 
{
  vwarning("Cloning VisusSharedValue makes no sense. Clone derived class instead. Returning NULL pointer");  
  return pVisusSharedValue();
} 

pVisusSharedValue VisusSharedValue::instantiate() const 
{
  vwarning("Instatiating VisusSharedValue makes no sense. Instantiate derived class instead. Returning NULL pointer");  
  return pVisusSharedValue();
} 

const char* VisusSharedValue::getName () const 
{
  vwarning("Non-sensical access to virtual baseclass");
  return NULL;
}

int VisusSharedValue::getIndex() const 
{
  vwarning("Non-sensical access to virtual baseclass");
  return -1;
}

std::string VisusSharedValue::getString() const 
{
  std::ostringstream ss;
  ss << getName() << "(" << this << ")";
  return ss.str();
}


void VisusSharedValue::toXML(XMLNode& parent, bool lock) const
{
  if (lock) {
    if (! lockToRead()) {
      vwarning("unable to lock VisusSharedValue before writing XML to output stream");
    }
  }
  
  toXML (parent);

  if (lock) {
    if (! unlockAfterRead()) {
      vwarning("unable to unlock VisusSharedValue after writing XML to output stream");
    }
  }
}


void VisusSharedValue::toXML(XMLNode& parent) const
{
  std::stringstream ss;
  ss << "Derived class type(" << getIndex() << ")did not override toXML functionality";
  vwarning(ss.str().c_str());
}

bool VisusSharedValue::fromXML(XMLNode& node, bool lock)
{
  if (lock) {
    if (! lockToWrite()) {
      vwarning("unable to lock VisusSharedValue before writing from XML");
    }
  }
  
  bool success = fromXML (node);

  if (lock) {
    if (! unlockAfterWrite()) {
      vwarning("unable to unlock VisusSharedValue after writing from XML");
    }
  }
  return success;
}

bool VisusSharedValue::fromXML(XMLNode& node)
{
  std::stringstream ss;
  ss << "Derived class type(" << getIndex() << ")did not override fromXML functionality";
  vwarning(ss.str().c_str());
  return false;
}
