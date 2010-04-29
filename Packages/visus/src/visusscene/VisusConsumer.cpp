/***********************************************************************
 * *
 * * Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
 * * Produced at the Lawrence Livermore National Laboratory  
 * * Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
 * * LLNL-CODE-406031.  
 * * All rights reserved.  
 * *   
 * * This file is part of "Simple and Flexible Scene Graph Version 2.0."
 * * Please also read BSD_ADDITIONAL.txt.
 * *   
 * * Redistribution and use in source and binary forms, with or without
 * * modification, are permitted provided that the following conditions are
 * * met:
 * *   
 * * @ Redistributions of source code must retain the above copyright
 * *   notice, this list of conditions and the disclaimer below.
 * * @ Redistributions in binary form must reproduce the above copyright
 * *   notice, this list of conditions and the disclaimer (as noted below) in
 * *   the documentation and/or other materials provided with the
 * *   distribution.
 * * @ Neither the name of the LLNS/LLNL nor the names of its contributors
 * *   may be used to endorse or promote products derived from this software
 * *   without specific prior written permission.
 * *   
 * *  
 * * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
 * * LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
 * * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * * NEGLIGENCE OR OTHERWISE) ARISING
 * *
 * ***********************************************************************/

#include "VisusConsumer.h"
#include "VisusMetricData.h"

VisusConsumer::VisusConsumer(int numberOfInputs)
{
  init(numberOfInputs);
}


VisusConsumer::~VisusConsumer()
{
  // To break up any circular references we set the smart pointers to
  // NULL before deleting them
  for (pVisusProducerList::iterator iter=mInputNodes.begin(); iter!=mInputNodes.end(); ++iter)
    (*iter) = pVisusProducer();
  for (VisusDataPtrList::iterator iter=mSinks.begin(); iter!=mSinks.end(); ++iter)
    (*iter) = NULL;
}

void VisusConsumer::init(const int numberOfInputs)
{
  if (numberOfInputs < 0) {
    vwarning("Consumer must have i>=0 inputs");
    mNrOfInputs = 0;
  }
  else {
    mNrOfInputs = numberOfInputs;
    mConnected.resize(mNrOfInputs);
    mInputNodes.resize(mNrOfInputs);
    mSinks.resize(mNrOfInputs);
    for (VisusDataPtrList::iterator iter=mSinks.begin(); iter!=mSinks.end(); ++iter)
      (*iter) = NULL;
    for (std::vector<bool>::iterator iter=mConnected.begin(); iter!=mConnected.end(); ++iter)
      (*iter) = false;
  }
}

bool VisusConsumer::setNumberOfInputs(const int numberOfInputs)
{
  if (mNrOfInputs != 0) {
    vwarning("Number of producer inputs for consumer has already been set. This attempt is ignored.");
    return false;
  }
  init(numberOfInputs);
  return true;
}

int VisusConsumer::synchronize(int n)
{
  if (n < -1) {
    vwarning("Index out of range no such input. Synchronization aborted");
    return 0;
  }
  else if (n == -1) 
    n = mNrOfInputs;
  else if (n > mNrOfInputs) {
    vwarning("Cannot synchronize all requested inputs. Index out of range");
    n = mNrOfInputs;
  }
  
  int flag = 1;

  mInputLock.writeLock();

  for (int i=0;i<n;i++) {
    if ((mInputNodes[i] != NULL) && (mSinks[i] != NULL)) {
      if (mInputNodes[i]->readLockProduct() == 0) {
        vwarning("Could not read-lock input. Synchronization incomplete.");
        flag *= 0;
        continue;
      }

      if (mInputNodes[i]->product()->id() != mSinks[i]->id()) {
        mSinks[i]->copyContent(mInputNodes[i]->product());
        mSinks[i]->id(mInputNodes[i]->product()->id());
        flag *= 2;
      }

      if (mInputNodes[i]->unlockProduct() == 0) {
        vwarning("Could not unlock input.");
        flag *= 0;
      }
    }
    else if (mInputNodes[i] == NULL) {
      // Removed the warning since using a node without input is a
      // valid use case, for example, using loadData instead (ptb)
      //vwarning("Input node not valid. Synchronization incomplete.");
    }
    else if (mSinks[i] == NULL){
      vwarning("Input buffer not valid. Synchronization incomplete.");}
  }

  mInputLock.unlock();

  updateInputDomain();

  return flag;
}

int VisusConsumer::connect(int i, pVisusProducer producer, VisusData* sink)
{  

  if ((i < 0) || (i >= mNrOfInputs)) {
    vwarning("Cannot connect input. Index out of range.");
    return 0;
  } 

  if (producer == NULL) {
    vwarning("Cannot connect NULL producer. Connection ignored.");
    return 0;
  }

  if (sink == NULL) {
    vwarning("Cannot connect NULL sink. Connection ignored.");
    return 0;
  }
  
  mInputNodes[i] = producer;
  mConnected[i] = true;
  mSinks[i] = sink;
  mSinks[i]->resetId();

  return 1;
}

bool VisusConsumer::isConnected(int i) const
{
  if ((i < 0) || (i > (int)mConnected.size()))
    return false;

  return mConnected[i];
}

int VisusConsumer::loadData(VisusData* data, int input)
{
  if (input > mNrOfInputs || input < 0) {
    vwarning("Cannot load VisusData. Input index is out-of-range.");
    return 0;
  }
  if (mSinks[input] == NULL) {
    vwarning("Data at given index is NULL.");
    return 0;
  }

  mInputLock.writeLock();

  if (!mSinks[input]->copyContent(data)) {
    mInputLock.unlock();
    return 0;
  }

  mSinks[input]->resetId();

  mInputLock.unlock();

  updateInputDomain();
  return 1;
}

VisusDataRequest VisusConsumer::latestRequest(int i) const
{
  VisusDataRequest current;

  if (!isConnected(i))
    return current;
  else 
    return mInputNodes[i]->latestRequest();
}


VisusConsumer::VisusConsumer()
{
  mNrOfInputs = 0;
}

void VisusConsumer::updateInputDomain()
{
  VisusBoundingBox domain_box,bbox;
  VisusMetricData* metric;

  // Make sure that the inital box is invalid
  domain_box[0] = 1;
  domain_box[3] = 0;

  for (int i=0;i<mNrOfInputs;i++) {
    
    //! If we have any data at all 
    if (mSinks[i] != NULL) {
      
      // See whether this data contains a bounding box
      metric = dynamic_cast<VisusMetricData*>(mSinks[i]);
      if (metric != NULL) { // If it does incorporate it
        metric->getDomainBoundingBox(bbox);
        if (bbox.valid()) {
          if (!domain_box.valid())
            domain_box = bbox;
          else
            domain_box += bbox;
        }
      }
    }
  }

  if (domain_box.valid())
    mInputDomain = domain_box;
}
      
