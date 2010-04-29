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


#include "VisusDataTransform.h"


VisusDataTransform::VisusDataTransform(VisusNodeType type, uint8_t number_of_inputs, VisusData* const product,
                                       VisusProcessingMode mode) :
  VisusGroup(type), VisusConsumer(number_of_inputs), VisusProducer(product), VisusSampleExtractor(), 
  mProcessingMode(mode), mDataID(number_of_inputs,NULLID), mStreamingInput(number_of_inputs,NULL)
{
}


VisusDataTransform::~VisusDataTransform()
{
}

int VisusDataTransform::checkData(const std::vector<VisusData*>& inputs)
{
  return 1;
}

void VisusDataTransform::display3D(VisusTransformation3D model_view_3D)
{
#ifdef VISUS_THREADLESS

  // If we are operating in threadless mode we use the display call to
  // signal production
  produceThreadless();

#endif

  VisusGroup::display3D(model_view_3D);
}


void* VisusDataTransform::extractionLoop(void* )
{
  uint8_t status;
  bool must_process;
  VisusProcessingMode mode;

  // Make sure the constructor of the thread is finished
  while (mThread == NULL) 
    sleepmillisec(sUpdateInterval);
	    
  // Indicate that we have started processing
  mThread->status(WORKING);

  // Until we get an outside signal to stop we continue
  while (!mThread->terminated()) {
    
    must_process = false;
    //must_process = true;

    //fprintf(stderr,"VisusDataTransform::extractionLoop   checkParameters\n");
    status = checkParameters();
    //fprintf(stderr,"\t status %d\n",status);

    // If the parameters we have are in valid
    if (status == 0) {
      sleepmillisec(sUpdateInterval); // We wait
      continue; // and try over again
    }
    else if (status == 2) // If the parameter have changed
      must_process = true;

    // We must use the same processing mode for cheching and
    // processing the data. Since the user is allowed to change the
    // mode at any time we save it into a temp variable
    mode = mProcessingMode;


    //fprintf(stderr,"VisusDataTransform::extractionLoop   checkData mode %d\n",mode);
    switch (mode) {
      
    case VISUS_INCREMENTAL_PROCESSING:
      status = checkDataIncrementally();
      break;
    case VISUS_STREAMING_PROCESSING:
      status = checkDataStreaming(); 
      // If successful this call will leave all our producers products
      // read-locked for us to access
      break;
    }
    //fprintf(stderr,"\t status %d  must_process %d\n",status,must_process);
     
    // If the incoming data is corrupted
    if (status == 0)  {
      sleepmillisec(sUpdateInterval); // We wait
      continue; // and try over again
    }
    else if (status == 2) //If some of the input data has changed
      must_process = true;

    if (!must_process) { // If nothing has changed
      
      // If in streaming mode make sure that we unlock in the incoming
      // producers
      if (mProcessingMode == VISUS_STREAMING_PROCESSING)
        unlockInputs();
     
      sleepmillisec(sUpdateInterval); // We wait
      continue; // and try over again
    }
    else {// Otherwise, we must re-process our data
      
      // We have to produce a new piece of data and for this we need
      // the lock
      if (this->writeLockProduct() == 0) { // If we can't get the lock
        vwarning("Could not obtain writeLock for production ... halting data processing");
        
        // We have to make sure that we unlock the incoming producers
        // if we are in streaming mode
        if (mProcessingMode == VISUS_STREAMING_PROCESSING)
          unlockInputs();

        sleepmillisec(sUpdateInterval); // We wait
        continue; // and try over again
      }
        
      
      //fprintf(stderr,"VisusDataTransform::extractionLoop   process\n");
      // Otherwise, we process the current data
      switch (mode) {
        
      case VISUS_INCREMENTAL_PROCESSING:
        status = process(this->mSinks);
        break;
      case VISUS_STREAMING_PROCESSING:
        status = process(mStreamingInput);
        break;
      }
      
      // If in streaming mode make sure that we unlock in the incoming
      // producers as soon as we can
      if (mProcessingMode == VISUS_STREAMING_PROCESSING)
        unlockInputs();

      // If we successfully updated our product
      if (status == 1) {
        // We need to indicate that we must be re-drawn
        this->markAsDirty(); 
      }

      unlockProduct();
    } // else (!must_process)      
    
  } // while (!mThread->terminated())
   
  mThread->status(FINISHED);  
  
  return NULL;
}

int VisusDataTransform::processThreadless()
{
  uint8_t status;
  bool must_process;
  VisusProcessingMode mode;

  must_process = false;
  
  status = checkParameters();
  
  // If the parameters we have are in valid
  if (status == 0) {
    return 0; // We indicate that we could produce
  }
  else if (status == 2) // If the parameter have changed
    must_process = true;
  
  // We must use the same processing mode for cheching and
  // processing the data. Since the user is allowed to change the
  // mode at any time we save it into a temp variable
  mode = mProcessingMode;
  
  
  switch (mode) {
    
  case VISUS_INCREMENTAL_PROCESSING:
    status = checkDataIncrementally();
    break;
  case VISUS_STREAMING_PROCESSING:
    status = checkDataStreaming(); 
    // If successful this call will leave all our producers products
    // read-locked for us to access
    break;
  }
  
  // If the incoming data is corrupted
  if (status == 0) {
    return 0; // We indicate that we could produce
  }
  else if (status == 2) //If some of the input data has changed
    must_process = true;
  
  // If we must re-process our data
  if (must_process) {
    
    // We have to produce a new piece of data and for this we need
    // the lock
    if (this->writeLockProduct() == 0) { // If we can't get the lock
      vwarning("Could not obtain writeLock for production ... halting data processing");
      
      // We have to make sure that we unlock the incoming producers
      // if we are in streaming mode
      if (mProcessingMode == VISUS_STREAMING_PROCESSING)
        unlockInputs();
      
      return 0; // we indicate that we couldn't produce
    }
    
    
    // Otherwise, we process the current data
    switch (mode) {
      
    case VISUS_INCREMENTAL_PROCESSING:
      status = process(this->mSinks);
      break;
    case VISUS_STREAMING_PROCESSING:
      status = process(mStreamingInput);
      break;
    }
    
    // If we successfully updated our product
    if (status == 1) {
      // We need to indicate that we must be re-drawn
      this->markAsDirty(); 
    }
  } // if (must_process)
  
  
  // If in streaming mode make sure that we unlock in the incoming
  // producers
  if (mProcessingMode == VISUS_STREAMING_PROCESSING)
    unlockInputs();
  
  return 1;    
}

VisusDataRequest VisusDataTransform::latestRequest() const
{
  VisusDataRequest current;

  getValue(current);

  return current;
}

int VisusDataTransform::checkDataIncrementally()
{
  uint8_t status;
  static uint8_t valid = 0;
  static uint8_t checked = false;
  
  // Synchronize all incoming data and copy it if necessary
  status = this->synchronize();

  // If there was a problem getting the data
  if (status == 0)
    return 0; // We pass on the error

  // If the data has not changed and has already been checked for
  // validity return the old test result
  if ((status == 1) && checked && (valid > 0)) {
    valid = 1;
    return 1;
  }

  checked = false;

  // Otherwise, see whether the data is valid
  valid = checkData(mSinks); 

  if (valid) {
    // Remember that we checked the data already
    checked = true;
    return valid;
  }
  else 
    return 0;
}


int VisusDataTransform::checkDataStreaming()
{
  static uint8_t valid = 0;
  static uint8_t checked = false;
  bool changed = false;
  int i;

  for (i=0;i<this->mNrOfInputs;i++) {

    // If the i'th input node is valid and we can successfully lock
    // it's product
    if ((mInputNodes[i] != NULL) &&
        (mInputNodes[i]->readLockProduct() == 1)) { 
      
      // Here we explicitly cast away the const'ness to be able to
      // interface with the mSinks list of the VisusConsumer. Note
      // that since the streaming inputs are only used for poduction
      // they really should not changed so teh spirit of the const
      // pointer is preserved.
      mStreamingInput[i] = const_cast<VisusData*>(mInputNodes[i]->product());
    }
    else // Otherwise this update is doomed 
      break;
  }
    
  // If all producers were valid and all products are now locked
  if (i == this->mNrOfInputs) {
    
    // Check whether any of the data has changed since the last time
    // we saw it
    for (i=0;i<this->mNrOfInputs;i++) {
 
      if (mStreamingInput[i]->id() != mDataID[i])
        changed = true; 
    }

    // If nothing has changed return the old test result
    if (!changed && checked)
      return valid;

    checked = false;
  
    // Otherwise, check the data
    valid = checkData(mStreamingInput);
    
    // Only if the data is valid do we want to copy id's
    if (valid > 0) {
      for (i=0;i<this->mNrOfInputs;i++) {
        mDataID[i] = mStreamingInput[i]->id();
      }

      // Remember that we checked the data already
      checked = true;

      // Note that this return statement leaves the producers
      // read-locked
      return 2;
      
    }
    else { // If the data is not valid
      unlockInputs(); // We must remove the locks
      return 0;
    }
      

  }
  else { // If the producers were somehow invalid we need to unlock
         // those we already locked
    
    // The last producer we tried we did not lock
    i--;

    // For all others
    while (i >= 0) {
      
      if (mInputNodes[i]->unlockProduct() != 1) {
        vwarning("Could not unlock producers in streaming data update.");
      }
      
      i--;
    }

    valid = 0;
    checked = false;

    return valid;
  }
}
  
int VisusDataTransform::unlockInputs()
{
  uint8_t flag = 1;

  for (int i=0;i<this->mNrOfInputs;i++) {
   
    if (mInputNodes[i] == NULL) {
      vwarning("Found NULL streaming input pointer.");
      flag = 0;
    }
    else {
      
      if (mInputNodes[i]->unlockProduct() == 0) {
        vwarning("Could not unlock streaming input.");
        flag = 0;
      }
    }
  }
  
  return flag; 
}
