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


#include "VisusSharedBoundingBox.h"
#include "VisusAssert.h"

const char* VisusSharedBoundingBox::sTypeName  = "BoundingBox";
const int   VisusSharedBoundingBox::sTypeIndex  = VISUS_BOUNDINGBOX_TYPEID;
VisusMutex  VisusSharedBoundingBox::sSharedValueMutex;
VisusAtomicCounter<short> VisusSharedBoundingBox::sAccessCount;

const int            VisusSharedBoundingBox::sNumTraversalSteps = 2;
const TraversalState VisusSharedBoundingBox::sTraversalSteps[] = {
  SYNC_WITH_CHILDREN,
  UPDATE_THE_PARENT,
};

template <>
std::string toString(const VisusBoundingBox* value)
{
  if (value == NULL)
    return "VisusBoundingBox (NULL)";
  
  const VisusBoundingBox& box = *value;
  
  std::ostringstream ss;
  ss << "VisusBoundingBox (" << value << "): " 
  	 << box[0] << "," << box[1] << "," << box[2] << "," 
  	 << box[3] << "," << box[4] << "," << box[5];
  return ss.str();
}		


VisusSharedBoundingBox::VisusSharedBoundingBox() : VisusSharedValue(), VisusBoundingBox()
{
}

VisusSharedBoundingBox::VisusSharedBoundingBox(const VisusSharedBoundingBox &bb) : 
  VisusSharedValue(bb), VisusBoundingBox(bb)
{
}

 
// Update the value of the node and/or of the child.
int VisusSharedBoundingBox::syncWithChild(const VisusSharedValue& the_child )
{
  if (lockToWrite() == 0) {
    vwarning("Could not lock Bounding Box for writing");
    return 0;
  }

  const VisusSharedBoundingBox& child = (const VisusSharedBoundingBox&)the_child ; 
  
  if (!child.valid()) {
    vwarning("Synchornizing with invalid child ignored");
    if (unlockAfterWrite() == 0) 
      vwarning("Could not unlock Bounding Box after writing");
    return 0;
  }
  else if (!valid()) {
    vwarning("Synchronizing invalid parent with valid child");
    child.get(mBBox);
    if (unlockAfterWrite() == 0) 
      vwarning("Could not unlock Bounding Box after writing");
    return 0;
  }
  
  *this += child;
  
  
  if (unlockAfterWrite() == 0) {
    vwarning("Could not unlock Bounding Box after writing");
    return 0;
  }
  else
    return 1;
}

std::string VisusSharedBoundingBox::getString() const
{
	return "Shared"+toString((VisusBoundingBox*) this);
}

// Output unction to be replaced with xml code.
bool VisusSharedBoundingBox::save(FILE * ostream){
    fprintf(ostream," < %s %f, %f, %f, %f, %f, %f > ", 
        getName (),mBBox[0],mBBox[1],mBBox[2],mBBox[3],mBBox[4],mBBox[5]);
    return true;
};


