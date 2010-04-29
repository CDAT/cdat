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


#ifndef VISUSSHAREDVALUE_H
#define VISUSSHAREDVALUE_H

#include <cstdlib>
#include <cstdio>
#include <vector>

struct XMLNode;

#include "VisusString.h"
#include "VisusTypeInfo.h"
#include "VisusManagedObject.h"
#include "VisusSmartPointer.h"
#include "VisusAtomicCounter.h"

enum TraversalState{
  UPDATE_THE_CHILDREN = 0,
  SYNC_WITH_CHILDREN,
  UPDATE_THE_PARENT,
  SYNC_WITH_PARENT,
};

class VisusSharedValue;
typedef VisusSmartPointer<VisusSharedValue> pVisusSharedValue;


//! Base clase for shared local variables in the scenegraph
/*! SharedValue is the common baseclass for all local variables that
*  can be shared throughout the scene graph.
*/
class VisusSharedValue : public VisusManagedObject
{
 public:
  
  friend class VisusGroup;

  /***************************************************************
   ******       Static Variables / Access Functions      *********
   **************************************************************/

  //! Number of non-abstract derived types
  static const int sNumTypes = VISUS_NUM_TYPEIDS;

  //! Return the number of subclasses that have been registered
  static int numTypes() {return sNumTypes;}

  //! Instantiate the subclass of the given index
  static pVisusSharedValue instantiate(int type_index);

  //! Return a pointer to a dummy instance of the given type
  static const pVisusSharedValue dummySharedValue(int type_index);
  
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/

  //! Default constructor
  VisusSharedValue() : VisusManagedObject() {}

  //! Copy constructor
  VisusSharedValue(const VisusSharedValue& value) : VisusManagedObject() {}

  //! Create a new instance of the same type initialized by the 
  //! copy constructor.
  virtual pVisusSharedValue clone() const;

  //! Create a new instance of the same type initialized by the
  //! default constructor
  virtual pVisusSharedValue instantiate() const;

  //! Destructor
  virtual ~VisusSharedValue() {}

  /***************************************************************
   ******       Access functionality                     *********
   **************************************************************/

  //! Return the string idenitfying the "type"
  /*! Return a pointer to the string which (uniquely) identifies
   *  the derived class.
   *  @return: pointer to the identifier
   */ 
  virtual const char* getName () const;
  
  //! Return the index of this type in the VisusGroup::mParameter array
  /*! Return the relative position of this type in the mParameter
   *  array stored in VisusGroup.
   *  @return: relative index of this type in the parameter array
   */
  virtual int getIndex() const;

	//! Returns a string which prints the value of the shared object
	virtual std::string getString() const;

  //! Returns whether this shared value should inherit by default 
  virtual bool inheritsByDefault() const {return false;}

  //! Locking a particular type of values to read
  static int lockToRead(int type_index);

  //! Unlocking a particular type of values after reading
  static int unlockAfterRead(int type_index);

  //! Locking a particular type of values to write
  static int lockToWrite(int type_index);

  //! Unlocking a particular type of values after writing
  static int unlockAfterWrite(int type_index);

  //! Locking an instance to read
  virtual int lockToRead() const {return 1;}

  //! Unlocking an instance after reading
  virtual int unlockAfterRead() const {return 1;}

  //! Locking an instance to write
  virtual int lockToWrite() const {return 1;}

  //! Unlocking an instance after writing
  virtual int unlockAfterWrite() const {return 1;}

  /***************************************************************
   ******    Functionality to read / write XML          *********
   **************************************************************/

  //! Function to read XML data and load into instance
  bool fromXML(XMLNode& parent, bool lock);

  //! Function to write XML representing instance to output stream
  void toXML(XMLNode& parent, bool lock) const;

  /***************************************************************
   ******       Traversal functionality                  *********
   **************************************************************/

  //! Return the number of steps performed during a traversal .
  virtual int nSteps() const   {return 0;};

  //! Return a pointer to the array storing the sequence of traversal
  //! states
  virtual const TraversalState* theSteps() const {return NULL;}

  // Functions called to syncronize the values between parent and children.
  // The return value is a boolean indicating success (true) or failure (false).
  // By default the function does nothing but may be overridden by a type 
  // requiring propagation of values.

  //! Synchronize my value with that of my parent
  /*! This function synchronizes the current value with that of its
   *  parent. If successful the function returns true, otherwise it
   *  will return false.
   *  @param parent: constant reference to the parent 
   *  @return: 1 if successful, 0 otherwise
   */
  virtual int syncWithParent(const VisusSharedValue& parent) {return 1;}

  //! Synchronize my value with that of my parent
  virtual int syncWithParent(const pVisusSharedValue& parent) {return syncWithParent(*parent);}

  //! Synchronize my value with that of a child
  /*! This function synchronizes the current value with that of one of
   *  its children. If successful the function returns true,
   *  otherwise it will return false.  
   *  @param parent: constant reference the child 
   *  @return: 1 if successful, 0 otherwise
   */
  virtual int syncWithChild (const VisusSharedValue& child) {return 1;}

  //! Synchronize my value with that of a child
  virtual int syncWithChild (const pVisusSharedValue& child) {return syncWithChild(*child);}

protected:

  //! Function to read XML data and load into instance
  virtual bool fromXML(XMLNode& parent);

  //! Function to write XML representing instance to output stream
  virtual void toXML(XMLNode& parent) const;

  //! Update sink with the current value 
  /*! This function performs a \b protected read of the current value
   *  and writes the result to sink. ValueClass is assumed to be a
   *  sub-class of the caller such that the caller can be cast into
   *  ValueClass. If the ViSUS is compiled with
   *  VISUS_NO_DYNAMIC_CAST=0 the cast will be performed dynamically
   *  and the read will be aborted if it fails. Again, the read is
   *  mutex protected so this function will try to lock the shared
   *  value for reading.
   *  @param sink: instance of a sub-class of the caller that stores 
   *               the current value
   *  @return 1 if successful, 0 otherwise
   */
  template<typename ValueClass>
  int getValueInternal(typename ValueClass::VisusValueClass &sink) const;

  //! Set the value to source.
  /*! This function performs a \b protected write of source to the 
   *  r. ValueClass is assumed to be a
   *  sub-class of the caller such that the caller can be cast into
   *  ValueClass. If the ViSUS is compiled with
   *  VISUS_NO_DYNAMIC_CAST=0 the cast will be performed dynamically
   *  and the write will be aborted if it fails. Again, the write is
   *  mutex protected so this function will try to lock the shared
   *  value for writing.
   *  @param source: instance of a sub-class of the caller that stores 
   *                 the value to which the caller should be updated.
   *  @return 1 if successful, 0 otherwise
   */
  template<typename ValueClass>
  int setValueInternal(const typename ValueClass::VisusValueClass &source);    
};

template<class SharedValueClass>
int VisusSharedValue::getValueInternal(typename SharedValueClass::VisusValueClass &sink) const
{
  vverbose("getInternalValue:VSV:Input: %s\n", VISUS_VSP_VERBOSE, this->getString().c_str());
  
#ifdef VISUS_NO_DYNAMIC_CAST
  
  // Note that we must upcast the this pointer to SharedValueClass in
  // order to assign its contents to sink. The reason is the
  // VisusSharedValue is not a derived class of VisusValueClass but
  // SharedValueClass is.
  const SharedValueClass* cast = static_cast<const SharedValueClass*>(this);
  
#else
  // Note that this cast is a side-cast in the class hierarchy which
  // the dynamic cast resolves for us
  //const typename SharedValueClass::VisusValueClass* cast = dynamic_cast<const typename SharedValueClass::VisusValueClass*>(this);
  const SharedValueClass* cast = dynamic_cast<const SharedValueClass*>(this);
  
  if (cast == NULL) {
    char msg[1024];
    sprintf(msg, "Dynamic cast in getValueIntenal failed. Getting with invalid type for (%s,%d)", this->getName(),this->getIndex());
    vwarning(msg);
    vverbose(msg,VISUS_VSP_VERBOSE,NULL);
    return 0;
  }

#endif
  vverbose("getValueInternal:SVC:Input: %s\n", VISUS_VSP_VERBOSE, toString(cast).c_str());    
  sink = (*cast);

  vverbose("getValueInternal:Output: %s\n", VISUS_VSP_VERBOSE, toString(&sink).c_str());    
  return 1;
}

template<class SharedValueClass>
int VisusSharedValue::setValueInternal(const typename SharedValueClass::VisusValueClass &source )
{
  vverbose("setValueInternal:Input: %s\n", VISUS_VSP_VERBOSE, toString(&source).c_str());  	
  
#ifdef VISUS_NO_DYNAMIC_CAST
  // While this dobule typecasting seems overly complicated is is
  // necessary. A VisusSharedValue is NOT a baseclass of
  // SharedValueClass::VisusValueClass but it is a baseclass of
  // SharedValueClass. Therefore, we must first upcast before
  // then downcast to use the operator= in VisusValueClass
  SharedValueClass* tmp = static_cast<SharedValueClass*>(this);
  typename SharedValueClass::VisusValueClass* cast = static_cast<typename SharedValueClass::VisusValueClass*>(tmp);

#else
  // Note that this cast is a side-cast in the class hierarchy which
  // the dynamic cast resolves for us
  SharedValueClass* tmp = dynamic_cast<SharedValueClass*>(this);
  typename SharedValueClass::VisusValueClass* cast = dynamic_cast<typename SharedValueClass::VisusValueClass*>(tmp);
  //typename SharedValueClass::VisusValueClass* cast = dynamic_cast<typename SharedValueClass::VisusValueClass*>(this);
  
  if (cast == NULL) {
    char msg[1024];
    sprintf(msg, "Dynamic cast in setValueIntenal failed. Setting with invalid type for (%s,%d)", this->getName(),this->getIndex());
    vwarning(msg);
    vverbose(msg,VISUS_VSP_VERBOSE,NULL);
    return 0;
  }

#endif

  *cast = source;
  vverbose("setValueInternal:VC:Output: %s\n", VISUS_VSP_VERBOSE, toString(cast).c_str());    
  vverbose("setValueInternal:SVC:Output: %s\n", VISUS_VSP_VERBOSE, this->getString().c_str());    
  
  return 1;
}



#endif
