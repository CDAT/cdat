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


#include "VisusBlockSampleExtractor.h"
#include "VisusSharedDataRequest.h"
#include "VisusSharedDataDescription.h"
#include "VisusSharedFieldIndex.h"
#include "DataSourceFactory.h"

VisusBlockSampleExtractor::VisusBlockSampleExtractor() : VisusProducer(&mData,mDataMutex) 
{
  declareParameter<VisusSharedDataRequest>();
  declareParameter<VisusSharedDataDescription>();
  declareParameter<VisusSharedFieldIndex>();

  mSynchronizationFlag = false;

  // Create a NullDataSource
  mDataSource = DataSourceFactory::make("NullDataSource");

  mThread = NULL;
  
  mThread = new VisusThread(VisusBlockSampleExtractor::threadStart,this);
  
  while (mThread->status() == INITIALIZING)
    sleepmillisec(sUpdateInterval);
  

  mStatus = VISUS_EXTRACTION_INVALID;
}


VisusBlockSampleExtractor::~VisusBlockSampleExtractor()
{
  // Deleting the thread. The destructor makes sure that the thread
  // exits nicely
  delete mThread;
}

void VisusBlockSampleExtractor::rotate(float x, float y) 
{
  VisusDataRequest local;
  VisusTransformation3D acc;
  accumulate3D(acc);

  // Make sure to modify a local request for thread-safty Note that
  // this is NOT ultimately thread-safe. While we cannot corrupt the
  // data when using get/setValue we introduce race-conditions (should
  // we change to per-node lock ? what happens to inherited values ?)
  getValue(local);
  local.transformation().rotate(acc,x,-y);
  local.transformation().normalize();
  setValue(local);
}

void VisusBlockSampleExtractor::translate(float x, float y) 
{
  VisusDataRequest local;
  VisusTransformation3D acc;
  accumulate3D(acc);

  // Make sure to modify a local request for thread-safty Note that
  // this is NOT ultimately thread-safe. While we cannot corrupt the
  // data when using get/setValue we introduce race-conditions (should
  // we change to per-node lock ? what happens to inherited values ?)
  getValue(local);
  local.transformation().translate(acc,x,-y);
  setValue(local);
}

void VisusBlockSampleExtractor::display(VisusTransformation3D model_view_3D,
                                        VisusTransformation2D model_view_2D,
                                        bool lock_graph)
{
  //fprintf(stderr,"VisusBlockSampleExtractor::display   status %d\n",mStatus);

  VisusTransformation3D local;
  VisusDataRequest request;
    
  // Update the shared values
  getValue(local);
  getValue(request);


  model_view_3D *= local;
  model_view_3D *= request.transformation();
  model_view_3D *= request.translateToCenter();

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadMatrixf(model_view_3D);

  if (drawBoundingBox()) {
    glColor3f(1,1,1);
    request.drawBoundingBox();
  }
  
  if (lock_graph && (lockToRead() == 0)) {
    vwarning("Could not lock graph for reading no further recursion in the display function");
    return;
  }
  
  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display(model_view_3D,model_view_2D,false);

  if (lock_graph && (unlockAfterRead() == 0)) 
    vwarning("Could not unlock graph after rendering");
  
  glPopMatrix();
}


void* VisusBlockSampleExtractor::extractionLoop(void *)
{
  VisusBlockData buffer; // This is where the data will be constructed
  VisusDataDescription dataset; // The name of the current data set
  VisusFieldIndex field_index; // The index of the current field we are extracting
  VisusDataRequest request; // The current request
  
  int stride_index; // on which set of strides are we working
  double field_range[2]; // Min/Max bounds of the field we extract
  std::vector<int> num_samples; // The number of samples in each dimension

  while (mThread == NULL) 
    sleepmillisec(sUpdateInterval);
    
  mThread->status(WORKING);

  while (!mThread->terminated()) {
    
    ////////////////////////////////////////////////////////////
    //////      Stage 1: Request Synchronization      //////////
    ////////////////////////////////////////////////////////////

    // Sync the dataset, the data request, and the field index with the 
    // values currently held in the shared structure
    getValue(mDataDescription);
    getValue(mRequest);
    getValue(mFieldIndex);

    // If the dataset has changed
    if (mDataDescription != dataset) {
      dataset = mDataDescription;

      // Delete the now obsolete data source
      delete mDataSource;
      
      // Indicate that we have not worked on this data at all
      stride_index = -1;

      // Create a new appropriate data source
      mDataSource = DataSourceFactory::make(dataset); 
    }
    
    // If the current data source for some reason is not valid
    if (!mDataSource->isValid()) {
      // Indicate that we cannot work on anything
      mStatus = VISUS_EXTRACTION_INVALID;
      
      vmessage("Data source not valid extraction will hold.\nDataDescriptor:\n%s\n",dataset.c_str());
      
      // Wait till hopefully new valid values come it
      sleepmillisec(sUpdateInterval);
      
      // and start over
      continue; 
    }


    // If the scalar function has changed
    if (mFieldIndex != field_index) {
      field_index = mFieldIndex;
      stride_index = -1;
    }

    // If the field index is out of range
    if ((field_index < 0) || (field_index >= mDataSource->numberOfFields())) {
      // Indicate that we cannot work on anything
      mStatus = VISUS_EXTRACTION_INVALID;
      
      vmessage("Field index %d out of range for this data set, cannot extract data",(int)field_index);
      // Wait till hopefully new valid values come it
      sleepmillisec(sUpdateInterval);
      
      // and start over
      continue; 
    }
    
    // If we have gotten a new request
    if (mRequest != request) {
      request = mRequest;
      stride_index = -1;
    }

    // If the current request is not valid
    if (!request.valid()) {
      mStatus = VISUS_EXTRACTION_INVALID;
      
      vwarning("DataRequest invalid cannot extract data");
      sleepmillisec(sUpdateInterval);
      
      continue; 
    }

    ////////////////////////////////////////////////////////////
    //////      Stage 2: Resolution Refinement        //////////
    ////////////////////////////////////////////////////////////

    // If we have already worked on the finest resolution requested
    if (stride_index == request.numberOfResolutions()-1) {
      // Indicate that we are done with the last request
      mStatus = VISUS_EXTRACTION_FINISHED;

      // Make clear that we have just verified our status
      mSynchronizationFlag = false;
      
      sleepmillisec(sUpdateInterval);
      continue; 
    }

    // Increase our target resolution
    stride_index++;

    // Set our status depending on whether we are starting or refining
    // this request
    if (stride_index == 0) 
      mStatus = VISUS_EXTRACTION_STARTED;
    else 
      mStatus = VISUS_EXTRACTION_REFINING;
      
    // Make clear that we just verified our status
    mSynchronizationFlag = false;
    

    ////////////////////////////////////////////////////////////
    //////      Stage 3: Preparing Buffer             //////////
    ////////////////////////////////////////////////////////////

    
    // Update all the information about the data we are going to
    // extract
    
    buffer.matrix(request.transformation()); // The location an orientation
    buffer.setExtent(request.extent());      // The region this data covers
    mDataSource->fieldRange(field_index,field_range,field_range+1); 
    buffer.setFieldRange(field_range[0],field_range[1]); // The minx,max bounds of the data

    // Determine the resolution that should be used 
    int strides[3];
    request.strides(stride_index,strides);

   // Now determine the number of samples that will be used in this query
    int sampling[3];
    if (mDataSource->getNonAlignedSampling(request.extent(),request.transformation(),
                                           strides,sampling) == 0) {
      vwarning("Cannot determine sampling density for request. Aborting data request.");
      sleepmillisec(sUpdateInterval);
      continue; 
    }      

    // Set the number of samples, and data type / samplesize and
    // resize the buffer to accommodate all samples
    if (buffer.setSampling(sampling,
                           mDataSource->fieldType(field_index),
                           mDataSource->fieldSampleSize(field_index)) == 0) {
      vwarning("Could not prepare buffer for reading data. Aborting extraction.");
      continue; 
    }

    
    ////////////////////////////////////////////////////////////
    //////      Stage 4: Extracting Data              //////////
    ////////////////////////////////////////////////////////////

    int end_strides[3];
    request.endStrides(end_strides);

    // Get the data
    if (mDataSource->getNonAlignedData(request.extent(),sampling,strides,
                                       end_strides,field_index,
                                       request.timeState(),request.transformation(),
                                       buffer.data(),buffer.mask()) == 0) {
      vwarning("Could not extract data. Aborting extraction");
      continue; 
    }
    
    // We have a new pieceof data
    buffer.incId();
      

   ////////////////////////////////////////////////////////////
    //////      Stage 5: Copying Data                 //////////
    ////////////////////////////////////////////////////////////

    if (mDataMutex.lock() == 0) {
      vwarning("Could not lock local data nothing copied");
      continue;
    }

    mData.swapContent(&buffer);
    mData.id(buffer.id());

    if (mDataMutex.unlock() == 0) {      
      vwarning("Could not unlock local data after swapping");
      continue;
    } 

  }

  mThread->status(FINISHED);

  return NULL;
}
  
void* VisusBlockSampleExtractor::threadStart(void *data) 
{
  ((VisusBlockSampleExtractor *)data)->extractionLoop(NULL); 
  return 0;
}
