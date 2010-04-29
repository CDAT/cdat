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


#include "VisusDataProbe.h"
#include "VisusSharedFieldIndex.h"
#include "VisusSharedTransformation2D.h"
#include "VisusSharedFont.h"
#include "VisusSharedDataDescription.h"
#include "VisusNullDataSource.h"
#include "VisusDataSourceFactory.h"
#include "VisusStdInt.h"
#include "VisusIntTime.h"

pVisusDataProbe VisusDataProbe::instantiate()
{
  return gObjectFactory.instantiate<VisusDataProbe>();
}


VisusDataProbe::VisusDataProbe() : VisusGroup(VISUS_DATA_PROBE), mIndex(3,-1), mPosition(3,0)
{
  declareParameter<VisusSharedFieldIndex>();  
  declareParameter<VisusSharedDataDescription>();
  declareParameter<VisusSharedTransformation2D>();
  declareParameter<VisusSharedFont>();
  declareParameter<VisusSharedGlobalTime>();
  
  mDataSource = new VisusNullDataSource();
  mRenderInfo = true;
  mData = new unsigned char[50];
  
  VisusTransformation2D trans;
  trans.translate(-0.95,-0.7);
  setValue(trans);
 }

VisusDataProbe::~VisusDataProbe()
{
  delete[] mData;
  delete mDataSource;
}


void VisusDataProbe::translate(float x, float y)
{
  VisusBoundingBox bbox;
  VisusTransformation3D acc;
  VisusTransformation3D matrix;
  VisusOpenGLState state;
  std::vector<float> trans;
  
  if (mDataLock.readLock() == 0) {
    vwarning("Could not lock data source for reading. Ignoring translation.");
    return;
  }

  if (mDataSource->isValid()) {
    mDataSource->domainBoundingBox(bbox);
    
    if (mDataLock.unlock() == 0) 
      vwarning("Could not unlock data source.");

    accumulate3D(acc);
    getValue(matrix);
    getValue(state);

    trans = matrix.translation(acc,state,x,-y);

    trans[0] *= (bbox[3] - bbox[0]);
    trans[1] *= (bbox[4] - bbox[1]);
    trans[2] *= (bbox[5] - bbox[2]);
  
    matrix[12] += trans[0];
    matrix[13] += trans[1];
    matrix[14] += trans[2];
    
    matrix[12] = MIN(bbox[3],MAX(bbox[0],matrix[12]));
    matrix[13] = MIN(bbox[4],MAX(bbox[1],matrix[13]));
    matrix[14] = MIN(bbox[5],MAX(bbox[2],matrix[14]));

    setValue(matrix);
      
    synchronize(true);
  }
  else {
    if (mDataLock.unlock() == 0) 
      vwarning("Could not unlock data source.");
  }
}

void VisusDataProbe::display3D(VisusTransformation3D model_view_3D)
{
  synchronize();
  
  if (mDataLock.readLock() == 0) {
    vwarning("Could not lock data probe content for reading.");
    return;
  }
  
  glMatrixMode(GL_MODELVIEW);
  
  glPushMatrix();
  glLoadMatrixf(model_view_3D);
  
  // If we have a valid data source 
  if (mDataSource->isValid()) {
    
    VisusBoundingBox bbox;

    mDataSource->domainBoundingBox(bbox);

    glColor3f(1,1,1);
    glDisable(GL_LIGHTING);
    
    glBegin(GL_LINES);

    glVertex3f(mPosition[0],mPosition[1],bbox[2]);
    glVertex3f(mPosition[0],mPosition[1],bbox[5]);

    glVertex3f(bbox[0],mPosition[1],mPosition[2]);
    glVertex3f(bbox[3],mPosition[1],mPosition[2]);

    glVertex3f(mPosition[0],bbox[1],mPosition[2]);
    glVertex3f(mPosition[0],bbox[4],mPosition[2]);

    glEnd();
  }
  
  if (mDataLock.unlock() == 0)
    vwarning("Could not unlock data probe content.");
    
  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display(model_view_3D);

  glPopMatrix();
}

void VisusDataProbe::display2D(VisusTransformation2D model_view_2D)
{

  if (mDataLock.readLock() == 0) {
    vwarning("Could not lock data probe content for reading.");
    return;
  }

  // If we should render the 2D text
  if (mRenderInfo) {
    VisusTransformation2D matrix2d;
    VisusFont font;

    getValue(matrix2d);
    getValue(font);
    mInfo.font(font);
    
    glMatrixMode(GL_MODELVIEW);
    
    glPushMatrix();
    glLoadIdentity();
    
    
    model_view_2D *= matrix2d;
    glTranslatef(model_view_2D[6],model_view_2D[7],0);

    mInfo.render(600);
    
    glPopMatrix();
  }
  
  
  if (mDataLock.unlock() == 0)
    vwarning("Could not unlock data probe content.");
    
  recurse(model_view_2D);
}

void VisusDataProbe::synchronize(bool must_update)
{
  VisusDataDescription dataset;
  VisusFieldIndex field_index;
  VisusGlobalTime current_time;
  VisusBoundingBox bbox;

  // First we see whether the data set must be updated
  getValue(dataset);

  // If our data set has changed
  if (dataset != mDataset) {
    // delete the old data source
    delete mDataSource;
    
    if (mDataLock.writeLock() == 0) {
      vwarning("Could not lock data probe content for writing.");
      return;
    }

    mDataset = dataset;
    mDataSource = VisusDataSourceFactory::make(mDataset);
    
    // Get the new domain box
    mDataSource->domainBoundingBox(bbox);
    
    // And make sure that our bouding box is up to date
    this->setValue(bbox);
    
    // Get the new time parameters
    current_time = mDataSource->timeInfo();
    
    // And update the shared parameter accordingly
    setValue(current_time);

    if (mDataLock.unlock() == 0)
      vwarning("Could not unlock data probe content.");

    must_update = true;
  }

  // Now we check whether we have changed our field index
  getValue(field_index);

  // If it has we must recompute the data
  if (field_index != mFieldIndex) {
    
    if (mDataLock.writeLock() == 0) {
      vwarning("Could not lock data probe content for writing.");
      return;
    }
    
    mFieldIndex = field_index;

    if (mDataLock.unlock() == 0)
      vwarning("Could not unlock data probe content.");

    must_update = true;
  }

  // Now check whether we have changed our time
  getValue(current_time);
  if (current_time != mCurrentTime)
  {
    if (mDataLock.writeLock() == 0) {
      vwarning("Could not lock data probe content for writing.");
      return;
    }
    
    mCurrentTime = current_time;

    if (mDataLock.unlock() == 0)
      vwarning("Could not unlock data probe content.");

    must_update = true;
  }


  if (must_update) {

    if (mDataLock.writeLock() == 0) {
      vwarning("Could not lock data probe content for writing.");
      return;
    }

    // If our current data source is not valid we clear our data
    if (!mDataSource->isValid()) {
      mIndex[0] = mIndex[1] = mIndex[2] = -1;
      mPosition[0] = mPosition[1] = mPosition[2] = 0; 
    }
    else { // otherwise we re-exactract
    
      // Get our current position
      
      VisusTransformation3D matrix;
      
      getValue(matrix);
      
      mPosition[0] = matrix[12];
      mPosition[1] = matrix[13];
      mPosition[2] = matrix[14];

      // extract the data
      mDataSource->accessSample(mPosition,mFieldIndex,mCurrentTime.time(),mIndex,mData);
      
      
      //fprintf(stderr,"VisusDataProbe::synchronize  <%f, %f, %f> at index [%d,%d,%d] \n",
      //        mPosition[0],mPosition[1],mPosition[2],mIndex[0],mIndex[1],mIndex[2]);
    }

    // Finally create the new text
    createInfoText(mIndex,mDataSource->fieldType(field_index));

    if (mDataLock.unlock() == 0)
      vwarning("Could not unlock data probe content.");

  }
}
      
    

int VisusDataProbe::createInfoText(std::vector<int> index,PvDataType type)
{
  char info[100];

  switch (type) {
  case PV_RAW:
    sprintf(info,"[%d,%d,%d] = RAW",index[0],index[1],index[2]);
    break;
  case PV_CHAR:
  case PV_UCHAR:
    sprintf(info,"[%d,%d,%d] = %d",index[0],index[1],index[2],mData[0]);
    break;
  case PV_INT16:
    sprintf(info,"[%d,%d,%d] = %d",index[0],index[1],index[2],((int16_t*)mData)[0]);
    break;
  case PV_UINT16:
    sprintf(info,"[%d,%d,%d] = %d",index[0],index[1],index[2],((uint16_t*)mData)[0]);
    break;
  case PV_INT:
    sprintf(info,"[%d,%d,%d] = %d",index[0],index[1],index[2],((int*)mData)[0]);
    break;
  case PV_INT32:
    sprintf(info,"[%d,%d,%d] = %d",index[0],index[1],index[2],((int32_t*)mData)[0]);
    break;
  case PV_UINT32:
    sprintf(info,"[%d,%d,%d] = %d",index[0],index[1],index[2],((uint32_t*)mData)[0]);
    break;
  case PV_INT64:
    sprintf(info,"[%d,%d,%d] = %Ld",index[0],index[1],index[2],((int64_t*)mData)[0]);
    break;
  case PV_UINT64:
    sprintf(info,"[%d,%d,%d] = %Ld",index[0],index[1],index[2],((uint64_t*)mData)[0]);
    break;
  case PV_FLOAT:
  case PV_FLOAT32:
    sprintf(info,"[%d,%d,%d] = %e",index[0],index[1],index[2],((float*)mData)[0]);
    break;
  case PV_FLOAT64:
    sprintf(info,"[%d,%d,%d] = %e",index[0],index[1],index[2],((double*)mData)[0]);
    break;
  case PV_RGB:
    sprintf(info,"[%d,%d,%d] = <%d,%d,%d>",index[0],index[1],index[2],mData[0],mData[1],mData[2]);
    break;
  case PV_RGBA:
    sprintf(info,"[%d,%d,%d] = <%d,%d,%d,%d>",index[0],index[1],index[2],mData[0],mData[1],mData[2],mData[3]);
    break;
  }

  mInfo.text(info);
  return 1;
}

