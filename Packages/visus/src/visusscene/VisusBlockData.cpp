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


#include <cstdlib>

#include "xmlParser.h"

#include "VisusBlockData.h"
#include "VisusAssert.h"
#include "VisusXMLInterface.h"

const uint8_t VisusBlockData::sBitMask[8] = {1,2,4,8,16,32,64,128};

VisusBlockData::VisusBlockData() : 
  VisusMetricData(VISUS_BLOCK_DATA), mData(NULL), mDataType(PV_RAW), mSampleSize(0), mMask(NULL),
  mExtent(3,0), mSamples(3,0), mMinFieldValue(0), mMaxFieldValue(0), mDataSize(0), mDataCapacity(0),
  mMaskSize(0), mMaskCapacity(0)
{
}


VisusBlockData::~VisusBlockData()
{
  if (mDataCapacity > 0)
    free(mData);
  
  if (mMaskCapacity > 0)
    free(mMask);
}

void VisusBlockData::getDomainBoundingBox(std::vector<double>& left_lower, std::vector<double>& right_upper) const
{
  left_lower[0] = mMatrix[12];
  left_lower[1] = mMatrix[13];
  left_lower[2] = mMatrix[14];

  mMatrix.transform(&mExtent[0],&right_upper[0]);
}

void VisusBlockData::getDomainBoundingBox(VisusBoundingBox& bbox) const
{
  bbox[0] = mMatrix[12];
  bbox[1] = mMatrix[13];
  bbox[2] = mMatrix[14];
  
  float tmp[3];
  tmp[0] = mExtent[0];
  tmp[1] = mExtent[1];
  tmp[2] = mExtent[2];

  mMatrix.transform(tmp,&bbox[3]);
}

void VisusBlockData::getFieldRange(double &min_f, double &max_f) const 
{
  min_f = mMinFieldValue;
  max_f = mMaxFieldValue;
}

VisusTransformation3D VisusBlockData::translateToCenter() const
{
  return translationMatrix(-mExtent[0]/2,-mExtent[1]/2,-mExtent[2]/2);
}


void VisusBlockData::dataType(PvDataType type) 
{
  mDataType = type;
  
  if (mDataType != PV_RAW) 
    mSampleSize = pv_size_of(mDataType);
}


void VisusBlockData::setFieldRange(double min_f, double max_f)
{
  mMinFieldValue = min_f;
  mMaxFieldValue = max_f;
}

void VisusBlockData::setDomainBoundingBox(const std::vector<double>& left_lower, 
                                          const std::vector<double>& right_upper)
{
  mMatrix = VisusTransformation3D();

  mMatrix[12] = left_lower[0];
  mMatrix[13] = left_lower[1];
  mMatrix[14] = left_lower[2];

  mExtent[0] = right_upper[0] - left_lower[0];
  mExtent[1] = right_upper[1] - left_lower[1];
  mExtent[2] = right_upper[2] - left_lower[2];
} 

void VisusBlockData::setDomainBoundingBox(const VisusBoundingBox& bbox)
{
  mMatrix = VisusTransformation3D();

  mMatrix[12] = bbox[0];
  mMatrix[13] = bbox[1];
  mMatrix[14] = bbox[2];

  mExtent[0] = bbox[3] - bbox[0];
  mExtent[1] = bbox[4] - bbox[1];
  mExtent[2] = bbox[5] - bbox[2];
} 

void VisusBlockData::mask(int i, bool bit)
{
  if (bit) {
    mMask[i>>3] |= sBitMask[i%8];
  }
  else {
    mMask[i>>3] &= ~sBitMask[i%8];
  }
}

int VisusBlockData::compactDimensions(int dim)
{
  if ((dim != 1) && (dim != 2)) {
    vwarning("Blockdata can be compacted to one or two dimensional data only.");
    return 0;
  }

  // First we count how many empty dimensions we have to see whether
  // this compactification is possible
  int count = 0;

  for (int i=0;i<3;i++) {
    if (mSamples[i] == 1)
      count++;
  }

  if ((count < 1) || ((count < 2) && (dim == 1))) { 
    vwarning("Too few trivial dimensions. Cannot compact data to line.");
    return 0;
  }


  // First we produce a single trivial dimension in the z-coordinate
  if (mSamples[2] != 1) { // If we are not already done
    
    if (mSamples[1] != 1) 
      swapDimensionXY();

    swapDimensionYZ();
  }
  else { // Even if the last dimension is already flat we still have
         // to fix the matrices to avoid jumping slices/lines
    mMatrix[10] = 1;
    mMatrix[14] = 0;
    
    mCellCentered[10] = 1;
    mCellCentered[14] = 0;
  }
    
  
  // If we need to copmact things to a single dimension we must also
  // produce a trivial dimension in the y-coordinate
  if ((dim == 1) && (mSamples[1] != 1)) 
    swapDimensionXY();

  return 1;
}


int VisusBlockData::reserveSpace()
{
  int num_samples = 1;  
  std::vector<int>::iterator it;

  for (it = mSamples.begin();it!=mSamples.end();it++)  
    num_samples *= *it;
  
  //fprintf(stderr,"Reserving space for %d sampels %d %d %d\n",num_samples,mSamples[0],mSamples[1],mSamples[2]);  
  mDataSize = num_samples*mSampleSize;

  if (mDataSize > mDataCapacity) {
    mDataCapacity = mDataSize;

    if (mData != NULL)
      delete[](mData);
    mData = new unsigned char[mDataCapacity];
    
    if ((mData == NULL) && (mDataCapacity > 0)) {
      vwarning("Could not allocate memory of data array. Creating empty data object");
      if (mMaskCapacity > 0) {
        free(mMask);
        mMask = NULL;
      }

      mMaskSize = 0;
      mMaskCapacity = 0;
      mDataSize = 0;
      mDataCapacity = 0;

      mSamples = std::vector<int>(3,0);

      return 0;
    }
  }

  // the mask has num_samples many BITS which translates into as most
  mMaskSize = (num_samples >> 3) + 1; // many bytes

  if (mMaskSize > mMaskCapacity) {
    mMaskCapacity = mMaskSize;

    if (mMask != NULL)
      delete[](mMask);
    mMask = new unsigned char[mMaskCapacity];

    if ((mMask == NULL) && (mMaskSize > 0)) {
      vwarning("Could not allocate memory of mask array. Creating empty data object");
      if (mData != NULL) {
        free(mData);
        mData = NULL;
      }
      mMaskSize = 0;
      mMaskCapacity = 0;
      mDataSize = 0;
      mDataCapacity = 0;

      mSamples = std::vector<int>(3,0);

      return 0;
    }
  }

  memset(mMask,0,mMaskSize);

  return 1;
}

int VisusBlockData::usedSamples()
{
  return mSamples[0]*mSamples[1]*mSamples[2];
}

int VisusBlockData::swapContent(VisusData* data)
{
  if (!readCompatible(data) || !data->readCompatible(this)) {
    vwarning("Data types not compatible. Cannot swap.");
    return 0;
  }

  VisusMetricData::swapContent(data);

  VisusBlockData* block_data;

  block_data = (VisusBlockData*) data;

  if ((block_data->mDataSize == 0) || (block_data->mMaskSize == 0)) {
    vwarning("Cannot swap block data. Source does not own its arrays.");
    return 0;
  }

  std::swap(this->mMatrix,block_data->mMatrix);
  std::swap(this->mCellCentered,block_data->mCellCentered);
  std::swap(this->mDataType,block_data->mDataType);
  std::swap(this->mSampleSize,block_data->mSampleSize);
  std::swap(this->mExtent,block_data->mExtent);
  std::swap(this->mSamples,block_data->mSamples);
  std::swap(this->mMinFieldValue,block_data->mMinFieldValue);
  std::swap(this->mMaxFieldValue,block_data->mMaxFieldValue);
  
  std::swap(this->mDataSize,block_data->mDataSize);
  std::swap(this->mDataCapacity,block_data->mDataCapacity);
  std::swap(this->mMaskSize,block_data->mMaskSize);
  std::swap(this->mMaskCapacity,block_data->mMaskCapacity);
 
  std::swap(this->mData,block_data->mData);
  std::swap(this->mMask,block_data->mMask);

  return 1;
}


int VisusBlockData::copyContent(const VisusData* data)
{
  if (!readCompatible(data)) {
    vwarning("Data type not recognized. Cannot copy.");
    return 0;
  }

  VisusMetricData::copyContent(data);
  
  const VisusBlockData* block_data = (const VisusBlockData*) data;


  // Now copy all the size independent information
  mCellCentered = block_data->mCellCentered;
  mDataType = block_data->dataType();
  mSampleSize = block_data->sampleSize();
  mSamples = block_data->samples();
  mExtent = block_data->extent();
  block_data->getFieldRange(mMinFieldValue,mMaxFieldValue);

  // Now make sure we locally have enough space to copy the data
  reserveSpace();
  
  //int num_samples = usedSamples();

  // Finally copy the data and mask array
  if (mData != NULL)
    memcpy(mData,block_data->data(),mDataSize);

  if (mMask != NULL)
    memcpy(mMask,block_data->getMask(),mMaskSize);
  
  return 1;
}

bool VisusBlockData::readCompatible(const VisusData* data) const
{
  if (data == NULL)
    return false;

  if (data->dataFormat() == VISUS_BLOCK_DATA)
    return true;
  
  return false;
}

void VisusBlockData::swapDimensionXY()
{
  std::swap(mExtent[0],mExtent[1]);
  std::swap(mSamples[0],mSamples[1]);

  mMatrix[0] = mMatrix[5];
  mMatrix[5] = 1;
  mMatrix[12] = mMatrix[13];
  mMatrix[13] = 0;

  mCellCentered[0] = mCellCentered[5];
  mCellCentered[5] = 1;
  mCellCentered[12] = mCellCentered[13];
  mCellCentered[13] = 0;

  /*
  std::swap(mMatrix[0],mMatrix[5]);
  std::swap(mMatrix[12],mMatrix[13]);

  std::swap(mCellCentered[0],mCellCentered[5]);
  std::swap(mCellCentered[12],mCellCentered[13]);  
  */
}

void VisusBlockData::swapDimensionYZ()
{
  std::swap(mExtent[1],mExtent[2]);
  std::swap(mSamples[1],mSamples[2]);

  mMatrix[5] = mMatrix[10];
  mMatrix[10] = 1;
  mMatrix[13] = mMatrix[14];
  mMatrix[14] = 0;
  
  mCellCentered[5] = mCellCentered[10];
  mCellCentered[10] = 1;

  mCellCentered[13] = mCellCentered[14];
  mCellCentered[14] = 0;

  /*
  std::swap(mMatrix[5],mMatrix[10]);
  std::swap(mMatrix[13],mMatrix[14]);

  std::swap(mCellCentered[5],mCellCentered[10]);
  std::swap(mCellCentered[13],mCellCentered[14]);  
  */
}


void VisusBlockData::toXMLLocalVariables(XMLNode& node) const
{
  VisusMetricData::localXMLVariables(node);

  XMLNode child = node.addChild("cellCentered");
  mCellCentered.toXML(child);
  
  node.addAttribute("dataType", mDataType);
  node.addAttribute("sampleSize", mSampleSize);
  node.addAttribute("dataSize", mDataSize);
  node.addAttribute("maskSize", mMaskSize);
  node.addAttribute("min", mMinFieldValue);
  node.addAttribute("max", mMaxFieldValue);

  addChild(node, "extent", mExtent);
  addChild(node, "samples", mSamples);

  saveXMLData(node);
}

void VisusBlockData::saveXMLData(XMLNode& node) const
{
  XMLNode data = node.addChild("data");
  XMLNode mask = node.addChild("mask");

  switch (VisusXMLInterface::sWriteXMLDataStorage)
  {
  case ASCII:
  {
    std::stringstream ssData;
    for (int i=0; i<mDataSize; ++i) {
      ssData << mData[i] << " ";
    }
    data.addText(ssData.str().c_str());

    std::stringstream ssMask;
    for (int i=0; i<mMaskSize; ++i) {
      ssMask << mMask[i] << " ";
    }
    mask.addText(ssMask.str().c_str());
  }
  break;
  case BASE64:
  {
    XMLParserBase64Tool base64;

    // Save Data As Binary
    XMLSTR encoded = base64.encode(mData, mDataSize);
    data.addText(encoded);
    
    // Save Mask As Binary
    encoded = base64.encode(mMask, mMaskSize);
    mask.addText(encoded);
  }
  break;
  }
}

bool VisusBlockData::fromXMLLocalVariables(XMLNode& node, XMLDataStorage storageType)
{
  if (! VisusMetricData::fromLocalXMLVariables(node))
    return false;

  vverbose("  load VisusBlockData xml\n", VISUS_XML_VERBOSE);

  XMLNode child = node.getChildNode("cellCentered");
  XMLNode n = child.getChildNode(0);
  if (! mCellCentered.fromXML(n)) {
    vwarning("Failed to load cell centered for VisusBlockData");
    return false;
  }
  
  mDataType = (PvDataType)xmltoi(node.getAttribute("dataType"), mDataType);
  mSampleSize = xmltoi(node.getAttribute("sampleSize"), mSampleSize);
  mDataSize = xmltoi(node.getAttribute("dataSize"), mDataSize);
  mMaskSize = xmltoi(node.getAttribute("maskSize"), mMaskSize);
  mMinFieldValue = xmltof(node.getAttribute("min"), mMinFieldValue);
  mMaxFieldValue = xmltof(node.getAttribute("max"), mMaxFieldValue);

  getChild(node, "extent", mExtent);
  getChild(node, "samples", mSamples);

  reserveSpace();

  return loadXMLData(node, storageType);
}


bool VisusBlockData::loadXMLData(XMLNode& node, XMLDataStorage storageType)
{
  XMLNode data = node.getChildNode("data");
  XMLNode mask = node.getChildNode("mask");

  switch (storageType)
  {
  case ASCII:
  {
    std::stringstream ssData;
    ssData << data.getText();
    for (int i=0; i<mDataSize; ++i) {
      ssData >> mData[i];
    }

    std::stringstream ssMask;
    ssMask << data.getText();
    for (int i=0; i<mMaskSize; ++i) {
      ssMask >> mMask[i];
    }
  }
  break;
  case BASE64:
  {
    XMLParserBase64Tool base64;

    // Save Data As Binary
    vverbose("  load data from xml size %d\n", VISUS_XML_VERBOSE, mDataSize);
    base64.decode(data.getText(), mData, mDataSize);
    
    // Save Mask As Binary
    vverbose("  load mask from xml size %d\n", VISUS_XML_VERBOSE, mMaskSize);
    base64.decode(mask.getText(), mMask, mMaskSize);
  }
  break;
  default:
    vwarning("unsupported storage type. failed to load data");
    return false;
  }
  vverbose("  block data loaded successfully\n", VISUS_XML_VERBOSE);
  return true;
}
