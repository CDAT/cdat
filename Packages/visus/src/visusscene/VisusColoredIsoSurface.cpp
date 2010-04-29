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

#include "VisusColoredIsoSurface.h"
#include "VisusSharedIsoValue.h"
#include "VisusSharedDataRequest.h"
#include "VisusStdInt.h"

#include "contour.h"


pVisusColoredIsoSurface VisusColoredIsoSurface::instantiate()
{
  return gObjectFactory.instantiate<VisusColoredIsoSurface>();
}

VisusColoredIsoSurface::VisusColoredIsoSurface() : VisusIsoSurface(VISUS_COLORED_ISOSURFACE, 2, 
                                                                   VISUS_STREAMING_PROCESSING)
{
  mSinks[CIS_COLOR_DATA] = &mColorData;
  mLocalData = NULL;

  this->threadInit();
}

VisusColoredIsoSurface::~VisusColoredIsoSurface()
{
  if (mLocalData!=NULL)
    delete [] mLocalData;
}

int VisusColoredIsoSurface::connectColor(pVisusProducer producer)
{
  if (connect(CIS_COLOR_DATA, producer, &mColorData) == 0) {
    vwarning("Could not connect color producer.");
    return 0;
  }
  
  return 1;
}


int VisusColoredIsoSurface::checkData(const std::vector<VisusData*>& inputs)
{
  VisusBlockData* iso_data;
  VisusBlockData* color_data;
  ConDataset* con_data;

  iso_data = static_cast<VisusBlockData*>(inputs[CIS_ISO_DATA]);
  color_data = static_cast<VisusBlockData*>(inputs[CIS_COLOR_DATA]);

  // Get Dimensions of Source Data
  std::vector<int> iso_dim = iso_data->samples();
  int dim[3];
  
  dim[0] = iso_dim[0];
  dim[1] = iso_dim[1];
  dim[2] = iso_dim[2];

  int nr_samples = dim[0] * dim[1] * dim[2];
  
  if (nr_samples == 0)
    return 0;

  
  if (iso_data->dataType() != color_data->dataType()) {
    vwarning("Data type of colored data (%d) does not match data type of iso data (%d)",
             color_data->dataType(),iso_data->dataType());
    return 0;	  
  }
  
  // Need To Combine Isosurface Data and Color Data  
  int contour_type = getContourType(iso_data->dataType(), nr_samples);
  
	if (mLocalData != NULL)
		delete [] mLocalData;
  mLocalData =  new unsigned char[2*nr_samples*iso_data->sampleSize()];
    
  memcpy(mLocalData, iso_data->data(), iso_data->dataSize());
  memcpy(mLocalData+iso_data->dataSize(), color_data->data(), MIN(color_data->dataSize(),iso_data->dataSize()));
  

  con_data = newDatasetReg(contour_type, CONTOUR_REG_3D, 2, 1, dim, mLocalData);
  
  if (con_data != NULL) {

    if (mContourData != NULL) {
      clearDataset(mContourData);
      mContourData = NULL;
    }
    mContourData = con_data;

    return 1;
  }
  else {
    vwarning("Could not create libcontour data set.");
    return 0;
  }
}


Contour3dData* VisusColoredIsoSurface::getContourData(ConDataset* contourData, const VisusIsoValue& isoValue)
{
  return getContour3d(contourData, CIS_ISO_DATA, 0, isoValue, CIS_COLOR_DATA);
}

void VisusColoredIsoSurface::toXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusHeightField did not receive top node");
  }

  node.addAttribute(VisusXMLInterface::sPRODUCER_TAG, id());

  if (mInputNodes[CIS_ISO_DATA] != NULL) {
    node.addAttribute("isoConnected", true);
    node.addAttribute("isoProducer", mInputNodes[CIS_ISO_DATA]->id());
  }
  else 
      node.addAttribute("isoConnected", false);
  

  if (mInputNodes[CIS_COLOR_DATA] != NULL) {
    node.addAttribute("colorConnected", true);
    node.addAttribute("colorProducer", mInputNodes[CIS_COLOR_DATA]->id());
  }
  else 
      node.addAttribute("isoConnected", false);
}

bool VisusColoredIsoSurface::fromXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusHeightField did not receive top node");
    return false;
  }

  // Connect as producer
  if (! visusXML::connectProducer<VisusColoredIsoSurface>(node, self())) {
    vwarning("failed to connect as producer when loading xml");
    return false;
  }
  
  // Get/Connect Iso Producer
  bool iso_connected = xmltobool(node.getAttribute("isoConnected"), false);
  if (iso_connected) {
    if (! visusXML::connectConsumer(node, "isoProducer", this, CIS_ISO_DATA, &mSourceData)) {
      vwarning("unable to connect to iso producer");
      return false;
    }
  }

  // Get/Connect Color Producer
  bool color_connected = xmltobool(node.getAttribute("colorConnected"), false);
  if (color_connected)
  {
	  if (! visusXML::connectConsumer(node, "colorProducer", this, CIS_COLOR_DATA, &mColorData)) {
		vwarning("unable to connect to color producer");
		return false;
	  }
  }

  return true;
}
