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


#ifndef VISUSSAMPLEEXTRACTOR_H
#define VISUSSAMPLEEXTRACTOR_H

#include "VisusDataSource.h"
#include "VisusThread.h"

enum VisusExtractionStatus {
  VISUS_EXTRACTION_STARTED = 0,  //! Work on the current request has
                                 //! been started but no results have
                                 //! been produced
  VISUS_EXTRACTION_REFINING = 1, //! The current request has produced
                                 //! some output and is being refined
  VISUS_EXTRACTION_FINISHED = 2, //! The currently stored request has
                                 //! been finished
  VISUS_EXTRACTION_INVALID = 3,  //! Waiting for a valid request
};



class VisusSampleExtractor 
{
public:

  //! Number of milliseconds the extractor will sleep when waiting
  static const int sUpdateInterval = 100;

  VisusSampleExtractor();
  
  virtual ~VisusSampleExtractor();

  VisusExtractionStatus status() {return mStatus;}
  
  //! Set the extraction status and reset the sync flag
  void status(VisusExtractionStatus status);

  //! Indicate whether the extractor is idle
  /*! Indicate whether the extractor is idle. Note that, this is not
   *  necessarily equivalent to a
   *  status()==VISUS_EXTRACTION_FINISHED. Since the thread only
   *  periodically checks for new requests it is possible that a new
   *  request has been posted (as VisusSharedDataRequest) but has not
   *  yet been loaded. The idle function handles this case explicitly
   *  by waiting for the next load cycle of the thread before making a
   *  decision. Furtermore, the idle function does \e not compare the
   *  posted with the current request as this would be an unnecessary
   *  concurrent access to the mRequest variable which then would need
   *  to be mutex protected. Finally, an extractor "working" on an
   *  invalid request is never idle.
   *  @return True if the extractor has finished with the most current
   *  request; false otherwise
   */
  bool idle();

protected:

  //! Initialization function that starts the extraction thread
  /*! Initialization function that starts the extraction thread. In
   *  general this \e cannot be moved into the constructor since the
   *  thread is likely to need some member variables
   *  initialized. Since the constructor of a baseclass is called
   *  before the constructor of a derived class starting the
   *  extraction thread in the constructor of VisusSampleExtractor
   *  will lead to problems. Each derived call should call
   *  threadInit() at the end of its constructor.
   */
  void threadInit();

  //! Pointer to the extractor thread
  VisusThread* mThread;

  //! Status flag to indicate the current stage in the extraction
  //! process
  VisusExtractionStatus mStatus;

  //! Flag that synchronizes the idle determination
  bool mSynchronizationFlag;
  
private:

  //! The actual extraction thread to be overloaded by derived classes
  virtual void* extractionLoop(void*) = 0;

  //! Static function to pass the extraxtionLoop into the pthread
  //! interface
  static void* threadStart(void* data);
};

#endif

