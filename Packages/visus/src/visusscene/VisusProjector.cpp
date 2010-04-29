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

#include "VisusProjector.h"

pVisusProjector VisusProjector::instantiate()
{
  return gObjectFactory.instantiate<VisusProjector>();
}


VisusProjector::VisusProjector() 
  : VisusDataTransform(VISUS_PROJECTOR_NODE,1,&mOutgoing), mProjection(NULL)
{
  this->threadInit();
}

  //! Destructore
VisusProjector::~VisusProjector()
{
}

int VisusProjector::checkParameters() 
{
  if (mProjection == NULL)
    return 0;

  int status;
  status = mProjection->updateShared(&(*this));

  //fprintf(stderr,"VisusProjector::checkParameters  status %d\n",status);
  return status;
}
    
      

int VisusProjector::checkData(const std::vector<VisusData*>& inputs)
{
  if (inputs[0] == NULL)
    return 0;

  VisusIndexedData* data;

  data = dynamic_cast<VisusIndexedData*>(inputs[0]);

  if (data == NULL) {
    //fprintf(stderr,"VisusProjector::checkData  status 0\n");
   return 0;
  }

  if (!mProjection->isCompatibleInput(data)) {
    //fprintf(stderr,"VisusProjector::checkData  status 0\n");
    return 0;
  }

  if (inputs[0]->id() != this->mDataID[0]) {
    //fprintf(stderr,"VisusProjector::checkData  status 2\n");
    return 2;
  }

  fprintf(stderr,"VisusProjector::checkData  status 1\n");
  return 1;
}

//! Set the projection
void VisusProjector::projection(const VisusProjection& projection)
{
  VisusProjection* tmp = mProjection;

 	mProjection = projection.clone();
  
  delete tmp;
}

int VisusProjector::connectInput(pVisusProducer producer)
{
  if (connect(0,producer,&mIncoming) == 0) {
    vwarning("Could not connect producer.");
    return 0;
  }

  return 1;
}


int VisusProjector::process(const std::vector<VisusData*>& inputs)
{
  int success;

  if (inputs[0] == NULL)
    return 0;

  success = mProjection->project(static_cast<VisusIndexedData*>(inputs[0]),
                                 &mOutgoing);

  // If our data had what we assume is a normal coming in
   if (mOutgoing.vertexDim() >= 6) {
     mOutgoing.computeVertexNormals();
  }

  this->mDataID[0] = inputs[0]->id();
  mOutgoing.id(inputs[0]->id());

  //fprintf(stderr,"VisusProjector::process   success %d  new id %d\n",success,mOutgoing.id());
  
  return success;
}

