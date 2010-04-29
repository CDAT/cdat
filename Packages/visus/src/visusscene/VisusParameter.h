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


#ifndef VISUSPARAMETER_H
#define VISUSPARAMETER_H

#include "VisusSharedValue.h"

//! Wrapper class to extend a shared value to a usable parameter
/*! This class is a wrapper that makes a shared value into a valid
 *  parameter which can be used throughout the graph. The difference
 *  between a parameter and a value is that parameters store whether
 *  they are inherited or not.
 *  
 *  WARNING !!! A parameter does NOT own the shared element whose
 *  pointer it stores. Values are created/destroyed/owned only by
 *  nodes. As such, the copy constructor purposefully will NOT perform
 *  a deep copy nor will the destructor deallocate the shared value.
 *
 */
class VisusParameter
{
public:

  friend class VisusGroup;

  //! Default reference used in case of errors
  static VisusParameter sNullReference;

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/
  
  //! Default constructor
  VisusParameter();

  //! Copy constructor
  /*! A copy constructor that on purpose does NOT perform a deep copy
   */ 
  VisusParameter(const VisusParameter& param);

  //! Default desctructor
  /*! Default destructor that on purpose does NOT deallocate the
   *  memory associated with the shared value.
   */
  ~VisusParameter() {}

  //! Assignment operator
  VisusParameter& operator=(const VisusParameter& param);

  //! Special "copy constructor" to clone a parameter
  /*! Special "copy constructor" that returns a parameter with the
   *  same shared value associated but with the inheritance flaged set
   *  independently of its own state. Note that it returns a parameter
   *  by value and therefore does not actually "construct" anything.
   */
  VisusParameter clone() const;

  //! Access to the default reference
  static VisusParameter& nullReference() {return sNullReference;}

  /***************************************************************
   ******       Access functionality                     *********
   **************************************************************/
  
private:

  //! Flag that indicates whether this shared value is inherited
  bool mInherit;

  //! Pointer to the actual element 
  pVisusSharedValue mElement;

  //! Return whether this value is inherited or not
  bool inherit() const {return mInherit;}
  
  //! Return the associated shared value
  pVisusSharedValue element() {return mElement;}

  //! Return the associated shared value
  const pVisusSharedValue element() const {return mElement;}

  //! Set the inheritance
  void inherit(bool b) {mInherit = b;}

  //! Associate the parameter with a different shared value 
  void element(const pVisusSharedValue& value) {mElement = value;}
};

#endif
