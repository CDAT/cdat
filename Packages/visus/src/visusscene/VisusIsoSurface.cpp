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

#include <sstream>

#include "VisusIsoSurface.h"
#include "VisusSharedIsoValue.h"
#include "VisusSharedDataRequest.h"
#include "VisusStdInt.h"

#include "contour.h"

#define NUM_VERTS 3
#define NUM_NORM  3 
#define NUM_COLOR 1
#define NUM_VARS NUM_VERTS + NUM_NORM + NUM_COLOR


pVisusIsoSurface VisusIsoSurface::instantiate()
{
  return gObjectFactory.instantiate<VisusIsoSurface>();
}

VisusIsoSurface::VisusIsoSurface() : VisusDataTransform(VISUS_ISOSURFACE,1,&mIsoSurface),
                                     mContourData(NULL), mIsoSurface(6,3), mIsoValue(-1000000)
{
  declareParameter<VisusSharedIsoValue>();
  mSinks[0] = &mSourceData;

  this->threadInit();
}

VisusIsoSurface::VisusIsoSurface(VisusNodeType type, int num_input, VisusProcessingMode mode) 
  : VisusDataTransform(type,num_input,&mIsoSurface,mode), mContourData(NULL),  mIsoSurface(5+num_input,3),
    mIsoValue(-1000000)
   
{
  declareParameter<VisusSharedIsoValue>();
  mSinks[0] = &mSourceData;
}

VisusIsoSurface::~VisusIsoSurface()
{
  if (mContourData != NULL) {
    clearDataset(mContourData);
    mContourData = NULL;
  }
}

VisusTranslationStyle VisusIsoSurface::translationStyle() const
{
  VisusDataRequest request;

  getValue(request);

  return request.transformation().translationStyle();
}


void VisusIsoSurface::translationStyle(VisusTranslationStyle style)
{
  VisusDataRequest request;

  getValue(request);
  request.transformation().translationStyle(style);
  setValue(request);
}

void VisusIsoSurface::translateRequest(float x, float y)
{
  VisusDataRequest request;
  VisusOpenGLState state;

  getValue(state);
  getValue(request);
  request.transformation().translate(VisusTransformation3D(),state,x,y);
  setValue(request);
}

int VisusIsoSurface::connectIso(pVisusProducer producer)
{
  if (connect(0,producer,&mSourceData) == 0) {
    vwarning("Could not connect producer.");
    return 0;
  }

  return 1;
}

int VisusIsoSurface::checkParameters()
{
  VisusIsoValue current_iso;

  getValue(current_iso);

  if (current_iso != mIsoValue) {
    mIsoValue = current_iso;
    return 2;
  }
  else 
    return 1;
}

int VisusIsoSurface::checkData(const std::vector<VisusData*>& inputs)
{
  VisusBlockData* source_data;

  // If no input is given 
  if (inputs[0] == NULL)
    return 0;

  source_data = static_cast<VisusBlockData*>(inputs[0]);

  if (source_data->id() == this->mDataID[0]) {
    //fprintf(stderr,"VisusIsoSurface::checkData unchanged\n"); 
    return 1;
  }

  if (mContourData != NULL) {
    clearDataset(mContourData);
    mContourData = NULL;
  }
    
  int dim[3];
  std::vector<int> samples; 

  samples = mSourceData.samples();
  dim[0] = samples[0];
  dim[1] = samples[1];
  dim[2] = samples[2];

  int contour_type = getContourType(source_data->dataType(), dim[0]*dim[1]*dim[2]);
  if (contour_type < 0) {
    //fprintf(stderr,"VisusIsoSurface::checkData could not create contour data set\n"); 
    return 0;
  }

  mContourData = newDatasetReg(contour_type,CONTOUR_REG_3D,1,1,dim,source_data->data());

  //fprintf(stderr,"VisusIsoSurface::checkData  created contour data set %d\n",mContourData);
  return 2;
}

  //! Call to process the data
int VisusIsoSurface::process(const std::vector<VisusData*>& inputs)
{
  Contour3dData* contour; 
  VisusBlockData* iso_data;

  contour = getContourData(mContourData, mIsoValue);
  
  if (contour == NULL) {
    vwarning("Isosurface extraction failed.");
    return 0;
  }
  
  mIsoSurface.reserveVertices(contour->nvert);
  mIsoSurface.reserveElements(contour->ntri);
  
  int i;
  for (i=0;i<contour->nvert;i++) {
    mIsoSurface.vertex(i)[0] = contour->vert[i][0];
    mIsoSurface.vertex(i)[1] = contour->vert[i][1];
    mIsoSurface.vertex(i)[2] = contour->vert[i][2];
    
    mIsoSurface.vertex(i)[3] = contour->vnorm[i][0];
    mIsoSurface.vertex(i)[4] = contour->vnorm[i][1];      
    mIsoSurface.vertex(i)[5] = contour->vnorm[i][2];
    
    if (contour->colorvar != NO_COLOR_VARIABLE && mIsoSurface.vertexDim() >=7 ) {
      mIsoSurface.vertex(i)[6] = contour->vfun[i];
    }
  }
  
  for (i=0;i<contour->ntri;i++) {
    mIsoSurface.element(i)[0] = contour->tri[i][0];
    mIsoSurface.element(i)[1] = contour->tri[i][1];
    mIsoSurface.element(i)[2] = contour->tri[i][2];
  }
  
  VisusDataRequest request;
  VisusTransformation3D matrix;
  VisusBoundingBox bbox;
  std::vector<int> samples;

  request = VisusConsumer::latestRequest(0);

  
  
  iso_data = static_cast<VisusBlockData*>(inputs[0]);


  //iso_data->getDomainBoundingBox(bbox);
  samples = iso_data->samples();
  bbox = request.domainBBox();

  matrix = translationMatrix(bbox[0],bbox[1],bbox[2]);
  matrix *= scaleMatrix((bbox[3] - bbox[0])/samples[0],(bbox[4] - bbox[1])/samples[1],
                                                   (bbox[5] - bbox[2])/samples[2]);
  mIsoSurface.matrix(matrix);
  
  mIsoSurface.incId();
  
  mIsoSurface.unit(iso_data->unit());
  

  return 1;
}

Contour3dData* VisusIsoSurface::getContourData(ConDataset* contourData, const VisusIsoValue& isoValue)
{
  return getContour3d(contourData, 0, 0, isoValue, NO_COLOR_VARIABLE);
}


int VisusIsoSurface::getContourType(const PvDataType dataType, const int numData)
{
  switch (dataType) 
  {
  case PV_CHAR:
  case PV_UCHAR:
    return CONTOUR_UCHAR;
    break;
  case PV_INT16:
  case PV_UINT16:
    return CONTOUR_USHORT;
    break;
  case PV_FLOAT:
  case PV_FLOAT32:
    return CONTOUR_FLOAT;
    break;
  case PV_FLOAT64:
    return CONTOUR_DOUBLE;
    break;
  default:
    {
      if (numData > 0)
      {
      	// Only warn for real data - meaning number of items > 0
      	std::stringstream ss;
    	  ss << "Iso-surface extraction not supported for this data type(" << dataType << "). Sorry.";
    	  vwarning(ss.str().c_str());
      }
    }
  } 
  return -1;
}


void VisusIsoSurface::displayBoundingBox() const
{
  VisusDataRequest request;
  
  request = VisusConsumer::latestRequest(0);
  
  request.drawBoundingBox();
}


void VisusIsoSurface::toXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusIsoSurface did not receive top node");
  }

  node.addAttribute(VisusXMLInterface::sPRODUCER_TAG, id());
  node.addAttribute("isoProducer", mInputNodes[0]->id());
}

bool VisusIsoSurface::fromXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusIsoSurface did not receive top node");
    return false;
  }

  // Connect us as producer
  if (! visusXML::connectProducer<VisusIsoSurface>(node, self())) {
    vwarning("failed to connect as producer while loading xml");
    return false;
  }

  // Get Iso Producer
  if (! visusXML::connectConsumer(node, "isoProducer", this, 0, &mSourceData)) {
    vwarning("failed to connect as consumer while loading xml");
    return false;
  }
  return true;
}
