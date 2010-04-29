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

#include "xmlParser.h"

#include "VisusMetricData.h"
#include "VisusAssert.h"

VisusMetricData::VisusMetricData(VisusDataFormat t) 
  : VisusData(t), mUnit(VISUS_INDEX_SPACE), mLeftLower(3,0), mRightUpper(3,0)
{
}

VisusMetricData::~VisusMetricData()
{
}

void VisusMetricData::getDomainBoundingBox(std::vector<double>& left_lower, 
                                           std::vector<double>& right_upper) const
{
  left_lower = mLeftLower;
  right_upper = mRightUpper;  
}

void VisusMetricData::getDomainBoundingBox(VisusBoundingBox& bbox) const
{
  bbox[0] = mLeftLower[0];
  bbox[1] = mLeftLower[1];
  bbox[2] = mLeftLower[2];
  bbox[3] = mRightUpper[0];
  bbox[4] = mRightUpper[1];
  bbox[5] = mRightUpper[2];
}

void VisusMetricData::setDomainBoundingBox(const std::vector<double>& left_lower, 
                                           const std::vector<double>& right_upper)
{
  mLeftLower = left_lower;
  mRightUpper = right_upper;
}

void VisusMetricData::setDomainBoundingBox(const VisusBoundingBox& bbox)
{
  mLeftLower[0] = bbox[0];
  mLeftLower[1] = bbox[1];
  mLeftLower[2] = bbox[2];
  
  mRightUpper[0] = bbox[3];
  mRightUpper[1] = bbox[4];
  mRightUpper[2] = bbox[5];
}



int VisusMetricData::swapContent(VisusData* data)
{
  VisusData::swapContent(data);

  VisusMetricData* metric_data;

#ifdef VISUS_NO_DYNAMIC_CAST
  
  metric_data = (VisusMetricData*)data;

#else
 
  metric_data = dynamic_cast<VisusMetricData*>(data);
  if (metric_data == NULL) {
    vwarning("Dynamic cast failed invalid data types.");
    return 0;
  }
#endif

  mMatrix = metric_data->mMatrix;
  this->mUnit = metric_data->mUnit;
  
  //std::swap(this->mLeftLower,metric_data->mLeftLower);
  //std::swap(this->mRightUpper,metric_data->mRightUpper);

  return 1;
}

int VisusMetricData::copyContent(const VisusData* data)
{
  VisusData::copyContent(data);

  const VisusMetricData* metric_data;

#ifdef VISUS_NO_DYNAMIC_CAST
  
  metric_data = (VisusMetricData*)data;

#else
 
  metric_data = dynamic_cast<const VisusMetricData*>(data);
  if (metric_data == NULL) {
    vwarning("Dynamic cast failed invalid data types.");
    return 0;
  }
#endif

  mMatrix = metric_data->mMatrix;
  this->mUnit = metric_data->mUnit;
  
  //this->mLeftLower = metric_data->mLeftLower;
  //this->mRightUpper = metric_data->mRightUpper;
  
  return 1;
}

void VisusMetricData::localXMLVariables(XMLNode& parent) const
{
  parent.addAttribute("unit", mUnit);
  mMatrix.toXML(parent);
}

bool VisusMetricData::fromLocalXMLVariables(XMLNode& node)
{
  if (strcmp(VisusData::XML_TAG, node.getName())) {
    vwarning("VisusMetricData did not recieve top level node");
    return false;
  }

  mUnit = (VisusUnit)xmltoi(node.getAttribute("unit"), mUnit);
  
  XMLNode n = node.getChildNode(VisusTransformation<3>::XML_TAG);
  if (! mMatrix.fromXML(n))
    return false;

  return true;
}
