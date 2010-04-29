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


#ifndef VISUSSHAREDBOUNDINGBOX_H
#define VISUSSHAREDBOUNDINGBOX_H

#include <cstdio>
#include <cstdlib>
#include "VisusSharedValue.h"
#include "VisusBoundingBox.h"
#include "VisusTypeInfo.h"

//! Helper function to return a string representing bounding box value
template <>
std::string toString(const VisusBoundingBox* value);


class VisusSharedBoundingBox : public VisusSharedValue, public VisusBoundingBox
{
  //! Ensure that all necessary type information are present 
  VISUS_DECLARE_COMBINED(VisusSharedBoundingBox,VisusBoundingBox);
  
public:
  
  /***************************************************************
   ******      Static Members                            *********
   **************************************************************/

  //! Number of steps during a traversal
  static const int sNumTraversalSteps;
  
  //! Array of traversal states
  static const TraversalState sTraversalSteps[];

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/

  VisusSharedBoundingBox();

  VisusSharedBoundingBox(const VisusSharedBoundingBox& bb);
  
  //! return the number of traversal states
  virtual int nSteps() const {return sNumTraversalSteps;}

  //! Access to the actual traversal steps
  virtual const TraversalState* theSteps() const {return sTraversalSteps;}
  
  //! Return string representing bounding box value
  virtual std::string getString() const;
  
  bool save(FILE * ostream = stderr);
  
  //! Synchronize my value with that of a child
  /*! This function synchronizes the current value with that of one of
   *  its children. If successful the function returns true,
   *  otherwise it will return false.  
   *  @param parent: constant reference the child 
   *  @return: 1 if successful, 0 otherwise
   */
  int syncWithChild(const VisusSharedValue& the_child);
  
};



#endif
