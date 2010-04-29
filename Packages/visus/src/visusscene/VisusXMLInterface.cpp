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
#include <algorithm>

#include "xmlParser.h"

#include "VisusConsumer.h"
#include "VisusXMLInterface.h"
#include "VisusAssert.h"


const char* VisusXMLInterface::sPRODUCER_TAG = "producerId";
XMLDataStorage VisusXMLInterface::sWriteXMLDataStorage = BASE64;
const float    VisusXMLInterface::sVersion = 1.0;
std::map<int, std::vector<VisusConsumerSink> > VisusXMLInterface::sConsumers;
std::map<int, pVisusProducer> VisusXMLInterface::sProducers;
std::map<std::string, std::string> VisusXMLInterface::sOldDDToNewDD;


bool VisusXMLInterface::write(const std::string filename, pVisusGroup root) const
{
  XMLNode xml = XMLNode::parseString("<Visus>");
  xml.addAttribute("version", sVersion);
  root->toXML(xml);
  vverbose("Finished building the XML tree\n", VISUS_XML_VERBOSE, NULL);

  XMLError e = xml.writeToFile(filename.c_str());
  if (e!=eXMLErrorNone) {
    vwarning("error writing XML tree to data file");
    vverbose("error writing XML tree to data file", VISUS_XML_VERBOSE);
    return false;
  }
  vverbose("Finished writing the XML file\n", VISUS_XML_VERBOSE, NULL);
  return true;
}

pVisusGroup VisusXMLInterface::read(const std::string filename)
{
  XMLNode xml = XMLNode::parseFile(filename.c_str());
  xml = xml.getChildNode();
  if (strcmp(xml.getName(), "Visus")) 
  {
    std::stringstream ss;
    ss << "invalid Visus XML file. top node was (" << xml.getName() << ") expected Visus";
    vwarning(ss.str().c_str());
    vverbose(ss.str().c_str(), VISUS_XML_VERBOSE);
    return pVisusGroup();
  }

  float version = xmltof(xml.getAttribute("version"));
  if (version != sVersion) 
  {
    std::stringstream ss;
    ss << "invalid version of XML given (" << version << "). Expected version (" << sVersion <<")";
    vwarning(ss.str().c_str());
    vverbose(ss.str().c_str(), VISUS_XML_VERBOSE);
    return pVisusGroup();
  }

  // VisusGroup Should Be Top Elements For SceneGraphs
  XMLNode xmlRoot = xml.getChildNode(0);
  VisusNodeType type = VisusGroup::validateXML(xmlRoot);
  if (type == VISUS_UNDEFINED_NODE) {
    vverbose("could not determine VisusGroup type. returning empty VisusGroup", VISUS_XML_VERBOSE);
    return pVisusGroup();
  }

  pVisusGroup root = gObjectFactory.createNode(type);
  bool success = root->fromXML(xmlRoot);
  if (! success) {
    vwarning("Failed to properly load XML file. encountered error(s)");
    vverbose("Failed to properly load XML file. encountered error(s)", VISUS_XML_VERBOSE);
    return pVisusGroup();
  }

  vverbose("Successfully finished loading XML file\n", VISUS_XML_VERBOSE);
  return root;
}

std::vector<std::string>  VisusXMLInterface::extractDataDescriptions(const std::string filename)
{
  XMLNode xml = XMLNode::parseFile(filename.c_str());
  xml = xml.getChildNode();
  if (strcmp(xml.getName(), "Visus")) 
  {
    std::stringstream ss;
    ss << "invalid Visus XML file. top node was (" << xml.getName() << ") expected Visus";
    vwarning(ss.str().c_str());
    vverbose(ss.str().c_str(), VISUS_XML_VERBOSE);
    return std::vector<std::string>();
  }


  std::vector<std::string> data_descriptions;
  std::vector<std::string>::iterator it;

  extractDataDescriptions(xml.getChildNode(0),data_descriptions);

  // Now remove all the duplicates
  std::sort(data_descriptions.begin(),data_descriptions.end());
  it = std::unique(data_descriptions.begin(),data_descriptions.end());
  data_descriptions.erase(it,data_descriptions.end());
  
  return data_descriptions;
}

void VisusXMLInterface::track(pVisusProducer producer, int id)
{
  std::map<int, pVisusProducer>::iterator iter = sProducers.find(id);
  if (iter != sProducers.end()) {
    vwarning("sProducers already contains specified id. ignoring this request");
    return;
  }
  vverbose("registering producer %d\n", VISUS_XML_VERBOSE, id);
  sProducers.insert(std::make_pair(id, producer));
}

void VisusXMLInterface::track(VisusConsumer* consumer, int id, int sink, VisusData* data)
{
  std::map<int, ConsumerSinkList>::iterator iter = sConsumers.find(id);
  if (iter == sConsumers.end()) {
    sConsumers.insert(make_pair(id, ConsumerSinkList()));
    iter = sConsumers.find(id);
  }
  vverbose("registering consumer with producer %d at sink %d\n", VISUS_XML_VERBOSE, id, sink);

  VisusConsumerSink cs;
  cs.consumer = consumer;
  cs.sink     = sink;
  cs.data     = data;

  iter->second.push_back(cs);
}

pVisusProducer VisusXMLInterface::getProducer(int id, bool& success)
{
  vverbose("requesting producer %d\n", VISUS_XML_VERBOSE, id);
  std::map<int, pVisusProducer>::iterator iter = sProducers.find(id);
  if (iter == sProducers.end()) {
    success = false;
    return pVisusProducer();
  }
  success = true;
  return iter->second;
}

ConsumerSinkList VisusXMLInterface::getConsumers(int id)
{
  vverbose("requesting consumers for producer %d\n", VISUS_XML_VERBOSE, id);
  std::map<int, ConsumerSinkList >::iterator iter = sConsumers.find(id);
  if (iter == sConsumers.end()) {
    return ConsumerSinkList();
  }
  return iter->second;
}


bool VisusXMLInterface::connectConsumers(pVisusProducer producer, int id)
{
  //! Check if any consumers already initialized use us
  ConsumerSinkList consumers = getConsumers(id);
  for(ConsumerSinkList::iterator iter=consumers.begin();
      iter!=consumers.end(); ++iter)
  {
    VisusConsumerSink& cs = (*iter);

    if (! cs.consumer->connect(cs.sink, producer, cs.data))
      return false;

    vverbose("successfully connected producer %d to consumer at sink %d\n", VISUS_XML_VERBOSE, id, cs.sink);
  }
  return true;
}

void VisusXMLInterface::registerDataDescription(const std::string oldDataDescription, 
                                                const std::string newDataDescription)
{
  vverbose("Registering oldDD(%s) as new value (%s)\n", VISUS_XML_VERBOSE, 
    oldDataDescription.c_str(), newDataDescription.c_str());
  sOldDDToNewDD.insert(std::make_pair(oldDataDescription, newDataDescription));
}

std::string VisusXMLInterface::getDataDescription(const std::string oldDataDescription)
{
  std::map<std::string, std::string>::iterator iter = sOldDDToNewDD.find(oldDataDescription);
  if (iter == sOldDDToNewDD.end()) {
    return "";
  }
  std::string value = iter->second;
  vverbose("Retrieving from oldDD(%s) new value (%s)\n", VISUS_XML_VERBOSE, oldDataDescription.c_str(), value.c_str());
  return value;
}

int VisusXMLInterface::extractDataDescriptions(const XMLNode& node, std::vector<std::string> &data_descriptions)
{
  // If we have found a data description
  if (strcmp(VisusDataDescription::XML_TAG, node.getName()) == 0) {
    
    // add it to the set
    data_descriptions.push_back(node.getText());
  }

  int flag = 1;
  for (int i=0;i<node.nChildNode();i++) 
    flag *= extractDataDescriptions(node.getChildNode(i),data_descriptions);
  
  return flag;
}

namespace visusXML {

bool connectConsumer(XMLNode& node, const char* attrName, 
                     VisusConsumer* consumer, const int sinkId, VisusData* sink)
{
  // Get Iso Producer ID
  int id = xmltoi(node.getAttribute(attrName), -1);
  if (id < 0) {
    vwarning("unable to retrieve id for producer");
    return false;
  }

  // Check If Producer Registered
  bool success = false;
  pVisusProducer producer = VisusXMLInterface::getProducer(id, success);
  if (success) {
    if (! consumer->connect(sinkId, producer, sink)) {
      return false;
    }
    vverbose("successfully connected producer %d to consumer at sink %d\n", VISUS_XML_VERBOSE, id, sinkId);
  }
  else {
    // Producer not registered yet, add us to its callback list
    VisusXMLInterface::track(consumer, id, sinkId, sink);
  }
  return true;
}

}

