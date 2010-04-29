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


#ifndef VISUSAXISALIGNEDEXTRACTOR_H
#define VISUSAXISALIGNEDEXTRACTOR_H

#include "VisusGroup.h"
#include "VisusProducer.h"
#include "VisusSampleExtractor.h"
#include "VisusGlobalTime.h"

class VisusAxisAlignedExtractor;
typedef VisusSmartPointer<VisusAxisAlignedExtractor> pVisusAxisAlignedExtractor;

class VisusAxisAlignedExtractor : public VisusGroup, public VisusProducer, public VisusSampleExtractor
{
public:

  static pVisusAxisAlignedExtractor instantiate();

  VisusAxisAlignedExtractor();

  VisusAxisAlignedExtractor(const VisusDataDescription& dataset);

  virtual ~VisusAxisAlignedExtractor();

  //! Return an info string identifying the node
  virtual std::string infoString() const {return std::string("Aligned extractor");}

  virtual const VisusDataSource* dataSource() const {return mDataSource;}

  virtual int interrupt();

  virtual void rotateRequest(float x, float y);

  virtual void translateRequest(float x, float y);

  virtual void displayBoundingBox() const;
  
  virtual void* extractionLoop(void*); 

  //! Process the current request without using threads
  /*! This function produces the product outside of the threaded
   *  normal production loop. This function is used to run the scene
   *  graph in a completely thread-free manner.
   */
  virtual int produce();

  //! Return the currently active request for our product
  virtual VisusDataRequest latestRequest() const;

protected:
  //! Store Attributes into XML stream
  void toXMLLocalVariables(XMLNode& parent);

  //! Load instance with XML attributes
  bool fromXMLLocalVariables(XMLNode& node);

  virtual void display3D(VisusTransformation3D model_view_3D = VisusTransformation3D());

private:

  //! Local data buffer storing the result of the last completed query
  VisusBlockData mData;

  //! Data descriptor
  VisusDataDescription mDataDescription;

  //! Pointer to the data source
  VisusDataSource* mDataSource;

  //! Currently queued data request
  VisusDataRequest mRequest;

  //! Current field index
  VisusFieldIndex mFieldIndex;

  //! Current time
  VisusGlobalTime mCurrentTime;
};


#endif

