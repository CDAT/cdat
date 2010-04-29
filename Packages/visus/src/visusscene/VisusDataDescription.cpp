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

#include "VisusAssert.h"
#include "VisusDataDescription.h"
#include "VisusIncoreEncoder.h"
#include "VisusXMLInterface.h"

const char* VisusDataDescription::XML_TAG = "VisusDataDescription";

VisusDataDescription::VisusDataDescription() : 
  std::string(),
  mSaveDataToXML(true)
{}

VisusDataDescription::VisusDataDescription(const char* str) : 
  std::string(str),
  mSaveDataToXML(true)
{}


void VisusDataDescription::toXML(XMLNode& parent) const
{
  XMLParserBase64Tool base64;

  XMLNode dd = parent.addChild(XML_TAG);
  dd.addText(c_str());
  dd.addAttribute("dataSaved", mSaveDataToXML);

  if (mSaveDataToXML && this->find("Incore") != std::string::npos)
  {
    VisusIncoreEncoder encoder(c_str());

    vverbose("VisusDataDescription incore data has %d fields\n", VISUS_XML_VERBOSE, encoder.numFields());
    for (int i=0; i<encoder.numFields(); ++i)
    {
      const int bufsize = encoder.getFieldSize(i);
      unsigned char* buffer = (unsigned char*) encoder.getFieldAddress(i);
      XMLNode child = dd.addChild("field");
      vverbose("VisusDataDescription encode incore field data size %d addr %d\n", VISUS_XML_VERBOSE,
        bufsize, (void*) buffer);
      XMLSTR encoded = base64.encode(buffer, bufsize);
      child.addText(encoded);
    }
  }
}

bool VisusDataDescription::checkRegistry(const std::string& value)
{
  std::string newValue = VisusXMLInterface::getDataDescription(value);
  if (newValue.length() > 0)
  {
    vverbose("VisusDataDescription (%s) overridden with registry match=>%s\n", VISUS_XML_VERBOSE, 
      value.c_str(), newValue.c_str());
    (*this) = newValue.c_str();
    return true;
  }
  return false;
}

bool VisusDataDescription::fromXML(XMLNode& node)
{
  XMLParserBase64Tool base64;

  if (strcmp(XML_TAG, node.getName())) 
  {
    vwarning("VisusDataDescription did not receive its top level node");
    vverbose("VisusDataDescription did not receive its top level node\n", VISUS_XML_VERBOSE);
    return false;
  }

  // Get Our Data Description
  std::string value = node.getText();
  vverbose("VisusDataDescription read xml descriptor=>%s\n", VISUS_XML_VERBOSE, value.c_str());

  // Check if a new data description was registered
  if (checkRegistry(value))
    return true;

  mSaveDataToXML = xmltobool(node.getAttribute("dataSaved"), mSaveDataToXML);

  if (value.find("Incore") != std::string::npos)
  {
    // Process Incore Data Description
    VisusIncoreEncoder encoder(value);

    // Check for registered data description with no specific address
    if (checkRegistry(encoder.toString(false))) {
      return true;
    }

    // Last chance is that we load saved data
    if (! mSaveDataToXML) {
      (*this) = "error-new data description not registered and incore data was not saved to XML";
      return false;
    }

    // Loop over and load each field
    for (int i=0; i<encoder.numFields(); ++i)
    {
      XMLNode child = node.getChildNode("field");

      const int bufsize = encoder.getFieldSize(i);
      unsigned char* buffer = new unsigned char[bufsize];

      vverbose("VisusDataDescription allocated buffer %d of size %d\n", VISUS_XML_VERBOSE, buffer, bufsize);
      base64.decode(child.getText(), buffer, bufsize);

      vverbose("VisusDataDescription adjusting field %d to address %d\n", VISUS_XML_VERBOSE,
          i, buffer);
      encoder.changeFieldAddress(i, (void*)buffer);
    }
    (*this) = encoder.toString().c_str();
  }
  return true;
}
