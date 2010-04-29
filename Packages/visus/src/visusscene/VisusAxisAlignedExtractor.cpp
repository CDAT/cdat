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


#include "VisusAxisAlignedExtractor.h"
#include "VisusSharedDataRequest.h"
#include "VisusSharedDataDescription.h"
#include "VisusSharedGlobalTime.h"
#include "VisusSharedFieldIndex.h"
#include "VisusDataSourceFactory.h"
#include "VisusNullDataSource.h"
#include "VisusIntTime.h"

pVisusAxisAlignedExtractor VisusAxisAlignedExtractor::instantiate()
{
  return gObjectFactory.instantiate<VisusAxisAlignedExtractor>();
}


VisusAxisAlignedExtractor::VisusAxisAlignedExtractor() 
  : VisusGroup(), VisusProducer(&mData), VisusSampleExtractor()
{
  declareParameter<VisusSharedDataRequest>();
  declareParameter<VisusSharedDataDescription>();
  declareParameter<VisusSharedFieldIndex>();
  declareParameter<VisusSharedGlobalTime>();

  mDataSource = new VisusNullDataSource(); 
  this->mNodeType = VISUS_ALIGNED_EXTRACTOR;

  // Initialize thread
  threadInit();
}

VisusAxisAlignedExtractor::~VisusAxisAlignedExtractor()
{
}

int VisusAxisAlignedExtractor::interrupt()
{
  if ((mDataSource == NULL) || (!mDataSource->isValid())) {
    // This happens all the time at startup so this warning gets
    // annoying (ptb)
    //vwarning("Cannot interrupt invalid data source");
    return 0;
  }

  return mDataSource->interrupt();
}

void  VisusAxisAlignedExtractor::rotateRequest(float x, float y)
{
}

void  VisusAxisAlignedExtractor::translateRequest(float x, float y)
{
  VisusDataRequest request;
  VisusBoundingBox bbox;
  VisusOpenGLState state;
  VisusTransformation3D acc;
  std::vector<float> trans;

  //fprintf(stderr,"VisusAxisAlignedExtractor::translate  %f %f \n",x,y);
  
  accumulate3D(acc);

  getValue(request);
  getValue(bbox);
  getValue(state);

  trans = request.transformation().translation(acc,state,x,-y);

  trans[0] *= (bbox[3] - bbox[0]);
  trans[1] *= (bbox[4] - bbox[1]);
  trans[2] *= (bbox[5] - bbox[2]);
  
  request.transformation()[12] += trans[0];
  request.transformation()[13] += trans[1];
  request.transformation()[14] += trans[2];

  setValue(request);
  
  // St any previous data extraction still running
  interrupt();

  // Indicate that we need to redraw the scenegraph
  markAsDirty();
}

void VisusAxisAlignedExtractor::displayBoundingBox() const
{
  VisusDataRequest request;
  
  //fprintf(stderr,"VisusAxisAlignedExtractor::displayBoundingBox().\n");

  getValue(request);
  
  request.drawBoundingBox();
}


void VisusAxisAlignedExtractor::display3D(VisusTransformation3D model_view_3D)
{
#ifdef VISUS_THREADLESS

  // If we are operating in threadless mode we use the display call to
  // signal production
  produce();

#endif

  VisusGroup::display3D(model_view_3D);
}

VisusDataRequest VisusAxisAlignedExtractor::latestRequest() const
{
  VisusDataRequest current;

  getValue(current);

  return current;
}

void* VisusAxisAlignedExtractor::extractionLoop(void *)
{
  VisusDataDescription dataset; // The name of the data set we are
                                // working on
  VisusFieldIndex field_index;  // The index of the current field we
                                // are extracting
  VisusGlobalTime current_time; // The time of field we are extracting
  VisusDataRequest request;     // The current request

  VisusDataSource *old_source;
  int stride_index;
  std::vector<int> strides(3); 
  std::vector<double> extent(3);
  VisusBoundingBox bbox; // The domain bounding box of the data
  VisusUnit unit; // The physical unit of the domain
  VisusBoundingBox query_region;

  // Make sure the constructor of the thread is finished
  while (mThread == NULL) 
    sleepmillisec(sUpdateInterval);
    
  // Indicate that we have started processing
  mThread->status(WORKING);

  // Until we get an outside signal to stop we continue
  while (!mThread->terminated()) {

    ////////////////////////////////////////////////////////////
    //////      Stage 1: Request Synchronization      //////////
    ////////////////////////////////////////////////////////////

    // Access the dataset, data request, and field index currently
    // stored in our parameter list
    getValue(mDataDescription);
    getValue(mRequest);
    getValue(mFieldIndex);
    getValue(mCurrentTime);

    // If the dataset has changed
    if (mDataDescription != dataset) {
      dataset = mDataDescription;

      // store the out-dated data source
      old_source = mDataSource;
      
      // Indicate that we have not worked on this data at all
      stride_index = -1;

      // Create a new appropriate data source
      mDataSource = VisusDataSourceFactory::make(dataset); 
      
      //sleepmillisec(100);
      //fprintf(stderr,"Continue exraction\n");

      // Store the data source specific domain attribute
      unit = mDataSource->unit();

      // Get the new domain box
      mDataSource->domainBoundingBox(bbox);

      // And make sure that our bouding box is up to date
      this->setValue(bbox);

      // Get the new time parameters
      mCurrentTime = mDataSource->timeInfo();

      // And update the shared parameter accordingly
      setValue(mCurrentTime);
        
      // Delete the now obsolete data source
      delete old_source;

    }

    // If the current data source for some reason is not valid
    if (!mDataSource->isValid()) {
      // Indicate that we cannot work on anything
      mStatus = VISUS_EXTRACTION_INVALID;

      // Make clear that we have just verified our status
      mSynchronizationFlag = false;
      
      vmessage("Data source not valid extraction will hold.\nDataDescriptor:\n%s\n",dataset.c_str());
      
      // Wait till hopefully new valid values come it
      sleepmillisec(sUpdateInterval);
      
      // and start over
      continue; 
    }

    // If we have a valid data source we want to make sure that the
    // request reflects the current bounding box.
    mRequest.domainBBox(bbox);

    // Copy the data source dependent information into mData. Note
    // that even though this information only changes when a new data
    // source is opened the copy here is necessary. Currently, some
    // data is passed on by swapping in which case the current unit
    // and domain bounding box might be invalid for the new data we
    // want to extract. Maybe for performance reasons this should be
    // changed later.
    mData.unit(unit);
    

    // If the scalar function has changed
    if (mFieldIndex != field_index) {
      field_index = mFieldIndex;
      stride_index = -1;
    }

    // If the field index is out of range
    if ((field_index < 0) || (field_index >= mDataSource->numberOfFields())) {
      // Indicate that we cannot work on anything
      mStatus = VISUS_EXTRACTION_INVALID;

      // Make clear that we have just verified our status
      mSynchronizationFlag = false;
      
      vmessage("Field index %d out of range for this data set, cannot extract data",(int)field_index);

      // Wait till hopefully new valid values come it
      sleepmillisec(sUpdateInterval);
      
      // and start over
      continue; 
    }
    
    // If the current time requested has changed
    if (mCurrentTime != current_time) {
 
      current_time = mCurrentTime;
      vverbose("VisusAxisAlignedExtractor - Time has changed!\n", VISUS_TIME_VERBOSE);

      // Indicate that we have not worked on this data at all
      stride_index = -1; 
    }

    /*
    // In the new implementation of VisusGlobalTime and the data
    // source the data source will automatically clamp any illegal
    // value to the closest valid one. Thus this test is no longer
    // necessary. However, this might trigger a "busy" wait if the
    // extractor continuously feeds incorrect data to the data
    // source. If this ever becomes an issue (it shouldn't since the
    // global shared time should reflect the data souce time) this
    // code needs to be re-activated. (ptb 03/05/09)

    // If the field index is out of range
    if ((current_time < mDataSource->timeBegin()) || 
        (current_time > mDataSource->timeEnd())) {
    
      vwarning("Current time %s out of range for this data set, cannot extract data",current_time.time().toString().c_str());

      // Indicate that we cannot work on anything
      mStatus = VISUS_EXTRACTION_INVALID;

      // Make clear that we have just verified our status
      mSynchronizationFlag = false;
      
      // Wait till hopefully new valid values come it
      sleepmillisec(sUpdateInterval);
      
      // and start over
      continue; 
    }
    */

    // If we have gotten a new request
    if (mRequest != request) {
      request = mRequest;
      setValue(mRequest);
      stride_index = -1;
    }

    // If the current request is not valid
    if (!request.valid()) {
      mStatus = VISUS_EXTRACTION_INVALID;

      // Make clear that we have just verified our status
      mSynchronizationFlag = false;
      
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
    
    strides = request.strides(stride_index);
    
    ////////////////////////////////////////////////////////////
    //////      Stage 4: Extracting Data              //////////
    ////////////////////////////////////////////////////////////

    // First we determined the actual bounding box of the query region
    // Since this is an axis-aligned extractor we assume that the
    // transformed extent is still axis aligned. Otherwise, we will
    // inflate the bbox.
    query_region = request.queryRegion();

    /*
    fprintf(stderr,"VisusAxisAlignedExtractor::extractionLoop   accessing Data\n");
    fprintf(stderr,"[%f,%f,%f] x [%f,%f,%f] at strides <%d,%d,%d>\n\n",
            query_region.leftLower()[0],query_region.leftLower()[1],query_region.leftLower()[2],
            query_region.rightUpper()[0],query_region.rightUpper()[1],query_region.rightUpper()[2],
            strides[0],strides[1],strides[2]);
    */
    
    // Access the data
    if (mDataSource->accessAlignedData(query_region.leftLower(),query_region.rightUpper(),
                                       request.startStrides(),request.endStrides(),strides,
                                       field_index, current_time.time(),
                                       mData, this->mProductLock) != 0) {
      
      // If the data access was successful then we have extracted a
      // valid piece of data and the mProductLock is *locked*

      // If the data is supposed to be lower dimensional we
      // potentially have to compact the dimensions and adjust the
      // matrices
      if (request.requestType() == VISUS_1D_REQUEST)
        mData.compactDimensions(1);
      else if (request.requestType() == VISUS_2D_REQUEST)
        mData.compactDimensions(2);

      if (this->mProductLock.unlock() == 0)
        vwarning("Could not unlock data after loading new data.");
   
      this->markAsDirty();
    }
  }

 
  mThread->status(FINISHED);  

  return NULL;
}


int VisusAxisAlignedExtractor::produce()
{  
  VisusDataDescription dataset; 
  VisusFieldIndex field_index; 
  VisusGlobalTime current_time;
  VisusDataRequest request;     
  VisusDataSource *old_source;
  VisusBoundingBox query_region;
  VisusBoundingBox bbox;
  std::vector<double> left_lower; // Left lower corner of the domain bounding box
  std::vector<double> right_upper; // Right upper corner o fthe domain bounding box
  VisusUnit unit; // The physical unit of the domain


  //fprintf(stderr,"VisusAxisAlignedExtractor::produce()\n");

  // flag to indicate whether we should produce at all
  bool must_produce = false;

  /* The following code is very similar to the code in the
   * extractinLoop except that the variables are used slighlty
   * differently. In the extraction loop the local variables in the
   * loop store the information of the previous extraction and the
   * member variables are used to synchronize with the shared
   * values. Here the opposite strategy is used. The member variables
   * store the last request data etc. while the local variables are
   * updated from the shared values.
   */


  // Access the dataset, data request, and field index currently
  // stored in our parameter list
  getValue(dataset);
  getValue(request);
  getValue(field_index);
  getValue(current_time);
  
  // If the dataset has changed
  if (mDataDescription != dataset) {
    mDataDescription = dataset;
    
    // store the out-dated data source
    old_source = mDataSource;
    
    // Create a new appropriate data source
    mDataSource = VisusDataSourceFactory::make(dataset); 
    
    // Store the data source specific domain attribute
    unit = mDataSource->unit();
    
    // Get the new domain box
    mDataSource->domainBoundingBox(bbox);
    
    // And make sure that our bouding box is up to date
    this->setValue(bbox);
    
    // Get the new time parameters
    mCurrentTime = mDataSource->timeInfo();
    
    // And update the shared parameter accordingly
    setValue(mCurrentTime);
    
    // Delete the now obsolete data source
    delete old_source;
    
    // indicate that we have to produce new data
    must_produce = true;
  }
  
  // If the current data source for some reason is not valid
  if (!mDataSource->isValid()) {
    
    vmessage("Data source not valid cannot produce.\nDataDescriptor:\n%s\n",dataset.c_str());
    
    // We cannot do anything
    return 0;
  }

  // If we have a valid data source we want to make sure that the
  // request reflects the current bounding box.
  mRequest.domainBBox(bbox);
  setValue(mRequest);

  
  
  // Store the data source specific domain attribute
  unit = mDataSource->unit();
  mDataSource->domainBoundingBox(left_lower,right_upper);

  // If the scalar function has changed
  if (mFieldIndex != field_index) {
    mFieldIndex = field_index;
    must_produce = true;
  }
  
  // If the field index is out of range
  if ((field_index < 0) || (field_index >= mDataSource->numberOfFields())) {
    
    vmessage("Field index %d out of range for this data set, cannot extract data",(int)field_index);
    
    return 0;
  }

  /* 
  // See above for comments
  // If the field index is out of range
  if ((current_time < mDataSource->timeBegin()) || 
      (current_time > mDataSource->timeEnd()))
  {
    vwarning("Current time out of range for this data set, cannot extract data");
    return 0;
  }
  */

  // If current time has changed
  if (mCurrentTime != current_time) {
    mCurrentTime = current_time;
    must_produce = true;
    vverbose("VisusAxisAlignedExtractor - Time has changed!\n", VISUS_TIME_VERBOSE);
  }  
  
  // If we have gotten a new request
  if (mRequest != request) {
    mRequest = request;
    must_produce = true;
  }
  
  // If the current request is not valid
  if (!request.valid()) {      
    vwarning("DataRequest invalid cannot extract data");
    
    return 0;
  }
  
  
  if (must_produce) {

    // Copy the data source dependent information into mData. Note
    // that even though this information only changes when a new data
    // source is opened the copy here is necessary. Currently, some
    // data is passed on by swapping in which case the current unit
    // and domain bounding box might be invalid for the new data we
    // want to extract. Maybe for performance reasons this should be
    // changed later.
    mData.unit(unit);
    //mData.setDomainBoundingBox(left_lower,right_upper);

    // First we determined the actual bounding box of the query region
    // Since this is an axis-aligned extractor we assume that the
    // transformed extent is still axis aligned. Otherwise, we will
    // inflate the bbox.
    query_region = request.queryRegion();
    
    // Access the data
    if (mDataSource->accessAlignedData(query_region.leftLower(),query_region.rightUpper(),
                                       request.startStrides(),request.endStrides(),request.endStrides(),
                                       field_index, current_time.time(),
                                       mData, this->mProductLock) != 0) {
      
      // If the data access was successful then we have extracted a
      // valid piece of data and the mProductLock is *locked*

      // If the data is supposed to be lower dimensional we
      // potentially have to compact the dimensions and adjust the
      // matrices
      if (request.requestType() == VISUS_1D_REQUEST)
        mData.compactDimensions(1);
      else if (request.requestType() == VISUS_2D_REQUEST)
        mData.compactDimensions(2);

      if (this->mProductLock.unlock() == 0)
        vwarning("Could not unlock data after loading new data.");
      
    }
    else {
      vwarning("Data access failed could not extract data.");
      
      return 0;
    }
  }
  
  return 1;
}

void VisusAxisAlignedExtractor::toXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusAxisAlignedExtractor did not receive top node");
  }

  node.addAttribute(VisusXMLInterface::sPRODUCER_TAG, id());

  // note: don't need to save block data as this will be regenerated 
  //   from data source
}

bool VisusAxisAlignedExtractor::fromXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusAxisAlignedExtractor did not receive top node");
    return false;
  }

  // Connect us as producer
  if (! visusXML::connectProducer<VisusAxisAlignedExtractor>(node, self())) {
    vwarning("failed to connect as producer when loading xml");
    return false;
  }
  return true;
}

