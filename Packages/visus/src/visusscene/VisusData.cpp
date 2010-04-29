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

#include "VisusData.h"
#include "VisusAssert.h"
#include "VisusXMLInterface.h"


const char* VisusData::XML_TAG = "VisusData";

VisusData::VisusData(VisusDataFormat t) : mDataFormat(t), mId(NULLID)
{
}

VisusData::~VisusData()
{
}


int VisusData::swapContent(VisusData* data)
{
  if (data == NULL) {
    vwarning("Cannot swap data with NULL pointer.");
    return 0;
  }

  std::swap(mDataFormat,data->mDataFormat);

  return 1;
}


int VisusData::copyContent(const VisusData* data)
{
  if (data == NULL) {
    vwarning("Cannot copy data from NULL pointer.");
    return 0;
  }

  mDataFormat = data->mDataFormat;

  return 1;
}

void VisusData::toXML(XMLNode& parent) const
{
  XMLNode data = parent.addChild(XML_TAG);
  data.addAttribute("format", mDataFormat);
  data.addAttribute("storage", VisusXMLInterface::sWriteXMLDataStorage);
  data.addAttribute("id", mId);

  toXMLLocalVariables(data);
}

bool VisusData::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusData did not receive top level node");
    return false;
  }

  mDataFormat = (VisusDataFormat)xmltoi(node.getAttribute("format"), mDataFormat);

  XMLDataStorage storage = (XMLDataStorage) xmltoi(node.getAttribute("storage"), 
                                                   VisusXMLInterface::sWriteXMLDataStorage);
  mId = xmltoi(node.getAttribute("id"), mId);

  return fromXMLLocalVariables(node, storage);
}
