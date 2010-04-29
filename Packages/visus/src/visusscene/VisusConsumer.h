/***********************************************************************
 * *
 * * Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
 * * Produced at the Lawrence Livermore National Laboratory  
 * * Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
 * * LLNL-CODE-406031.  
 * * All rights reserved.  
 * *   
 * * This file is part of "Simple and Flexible Scene Graph Version 2.0."
 * * Please also read BSD_ADDITIONAL.txt.
 * *   
 * * Redistribution and use in source and binary forms, with or without
 * * modification, are permitted provided that the following conditions are
 * * met:
 * *   
 * * @ Redistributions of source code must retain the above copyright
 * *   notice, this list of conditions and the disclaimer below.
 * * @ Redistributions in binary form must reproduce the above copyright
 * *   notice, this list of conditions and the disclaimer (as noted below) in
 * *   the documentation and/or other materials provided with the
 * *   distribution.
 * * @ Neither the name of the LLNS/LLNL nor the names of its contributors
 * *   may be used to endorse or promote products derived from this software
 * *   without specific prior written permission.
 * *   
 * *  
 * * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * * A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
 * * LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
 * * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * * NEGLIGENCE OR OTHERWISE) ARISING
 * *
 * ***********************************************************************/

#ifndef VISUSCONSUMER_H
#define VISUSCONSUMER_H

#include "VisusProducer.h"
#include "VisusData.h"
#include "VisusDataRequest.h"
#include "VisusBoundingBox.h"

class VisusConsumer;
typedef VisusSmartPointer<VisusConsumer> pVisusConsumer;

class VisusConsumer
{
public:

   friend class VisusSmartPointer<VisusConsumer>;
  
  //! Standard constructor
  VisusConsumer(int numberOfInputs);

  //! Destructor
  virtual ~VisusConsumer();

  //! Synchronize the first \e n inputs 
  /*! Synchronize the first \e n inputs or all of them if n = -1.
   *  @param n: number of inputs to be synchronized -1 for all existing
   *            pipes
   *  @return >1 if successful change; 1 if successfull no-change; 0 otherwise
   */
  virtual int synchronize(int n = -1);  

  //! Connect the given producer as i'th input to the given sink
  /*! Connect the given producer as i'th input to the given sink. The
   *  function will fail if either producer or consumer are invalid.
   *  @param i: index of the input that should be used for the
   *            connection 
   *  @param producer: smart pointer to a data producer
   *  @param sink: pointer to the sink @return 1 if successful; 0
   *  otherwise
   */
  virtual int connect(int i, pVisusProducer producer, VisusData* sink);


  //! Directly Load VisusData into the Consumer (returns true on success / otherwise false)
  virtual int loadData(VisusData* data, int input);

  //! Return true if producer for given index has been connected
  bool isConnected(int i) const;
  
protected:
  
  typedef std::vector<pVisusProducer> pVisusProducerList;
  typedef std::vector<VisusData*>     VisusDataPtrList;

  //! Return the currently active request of the i'th input
  /*! To preserve interactivity or at least the illusion of
   *  interactivity we often want to draw old data using a newer
   *  request. Requests ususally don't change significantly between
   *  frames and this allows ti interactively \e see the request move
   *  independently of how long it takes to actually extract a piece
   *  of data. The request function call will try to determine the
   *  currently active request for the i'th input by querying the
   *  corresponding producer. If such a producer does not exist it
   *  will return the default request.
   *  @param i index of the input whose request we are interested in
   *  @return Latest data request of the i'th input or a default request
   */
  VisusDataRequest latestRequest(int i) const;

 /*! Set the number of inputs dynamically, locked down in protected so that
  *  it is not generally available to end user.
  */
  bool setNumberOfInputs(const int numberOfInputs);

  //! The number of inputs this processing node accepts
  int mNrOfInputs;

  //! Pipes providing the inputs
  pVisusProducerList mInputNodes;

  //! Where to copy the inputs to
  VisusDataPtrList mSinks;

  //! Track whether producer for data was connected
  std::vector<bool> mConnected;

  //! Read write lock protecting the inputs
  VisusReadWriteLock mInputLock;

  //! The aggregated domain bounding of all metric inputs
  VisusBoundingBox mInputDomain;
  
private:
  //! No default constructor allowed
  VisusConsumer();

  void init(const int numberOfInputs);

  //! Recompute the domain bounding box
  void updateInputDomain();
};


#endif


