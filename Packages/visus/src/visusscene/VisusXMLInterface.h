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
#ifndef VISUS_XML_INTERFACE_H
#define VISUS_XML_INTERFACE_H

#include <string>
#include <map>
#include <vector>

#include "xmlParser.h"
#include "VisusGroup.h"

class VisusData;
class VisusConsumer;

class VisusProducer;
typedef VisusSmartPointer<VisusProducer> pVisusProducer;


enum XMLDataStorage
{
  ASCII=0,
  BASE64,
  EXTERNAL_FILE
};

typedef struct 
{
  int sink;
  VisusConsumer* consumer;
  VisusData*     data;
} VisusConsumerSink;

typedef std::vector<VisusConsumerSink> ConsumerSinkList;

class VisusXMLInterface
{
public:

  static const char* sPRODUCER_TAG;
  static XMLDataStorage sWriteXMLDataStorage;
  static const float sVersion;

  //! Write the scene graph in XML to specified filename
  bool write(const std::string filename, pVisusGroup root) const;

  //! Read the given XML file and return the scene graph
  pVisusGroup read(const std::string filename);

  //! Return all data descriptors used in the XML file
  std::vector<std::string> extractDataDescriptions(const std::string filename);

  //! Register new data description for a loaded data description
  static void registerDataDescription(const std::string oldDataDescription, const std::string newDataDescription);

#ifndef SWIG
  //! Track consumer
  static void track(VisusConsumer* consumer, int id, int sink, VisusData* data);

  //! Retrieve producer
  static pVisusProducer getProducer(int id, bool& success);

  //! Track producer
  static void track(pVisusProducer producer, int id);

  //! Callback on registered consumers for producer and connect
  static bool connectConsumers(pVisusProducer producer, int id);

  //! Retrieve consumer
  static ConsumerSinkList getConsumers(int id);

  //! Retrieve new data description for given data description
  static std::string getDataDescription(const std::string oldDD);

  //! Recursively look for data descriptions
  static int extractDataDescriptions(const XMLNode& node, std::vector<std::string> &data_descriptions);

#endif

private:

  static std::map<int, ConsumerSinkList >   sConsumers;
  static std::map<int, pVisusProducer>      sProducers;
  static std::map<std::string, std::string> sOldDDToNewDD;
};


#ifndef SWIG

namespace visusXML {

template <class T>
bool connectProducer(XMLNode& node, VisusSmartPointer<T> tProducer)
{
  pVisusProducer producer = tProducer;

  // Get our producer id
  int id = xmltoi(node.getAttribute(VisusXMLInterface::sPRODUCER_TAG), -1);
  if (id < 0) {
    vwarning("unable to retrieve our producerId to register with consumers");
    return false;
  }

  // Track us for possible future consumers
  VisusXMLInterface::track(producer, id);

  // Connect to already registered consumers
  if (! VisusXMLInterface::connectConsumers(producer, id)) {
    vwarning("failed to connect to consumers");
    return false;
  }

  return true;
}

//! Extract consumer from XML and track/connect to producers
bool connectConsumer(XMLNode& node, const char* attrName, 
                     VisusConsumer* consumer, const int sinkId, VisusData* sink);

}

#endif

#endif
