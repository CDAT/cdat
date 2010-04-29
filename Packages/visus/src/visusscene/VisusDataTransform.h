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

#ifndef VISUSDATATRANSFORM_H
#define VISUSDATATRANSFORM_H

#include "VisusGroup.h"
#include "VisusProducer.h"
#include "VisusConsumer.h"
#include "VisusSampleExtractor.h"

enum VisusProcessingMode {
  VISUS_INCREMENTAL_PROCESSING = 0,
  VISUS_STREAMING_PROCESSING   = 1
};


class VisusDataTransform;
typedef VisusSmartPointer<VisusDataTransform> pVisusDataTransform;

//! Baseclass for all nodes that follow the read data -> process ->
//! provide result model
/*! A VisusDataTransform implements the shared functionality of the
 *  read->process->publish model. A data transform implements a four
 *  step data processing loop taking care of all mutex locks involved.
 *  The four steps are:
 *
 *  1. Check whether any of the steering parameters have changed
 *  2. Check for new data
 *  3. Process new data
 *  4. Write the results
 *
 *  The primary assumption that must be met for this class to work
 *  properly is the following. All parameters that influence the
 *  processing must be *internal* to this class and unmodifyable
 *  outside Step 1.. The typical model would be for them to be private
 *  members of a derived class that *only* store the last known shared
 *  value. Thus Step 1. would do a number of getValue calls and if
 *  necessary update the internal parameter and Step 3 would process
 *  the data with these exact parameters. In particular, one should
 *  *not* make the parameters accesible via set/get functions as these
 *  would represent a significant threading hazard.
 *
 *  WARNING: Durin gthe constructor of a class virtual functions do
 *  *not* work meaning, during the constructor of a baseclass the
 *  *this object is not yet of type derivedclass (see
 *  http://www.parashift.com/c++-faq-lite/ctors.html#faq-10.7 ). Since
 *  the underlying thread relies heavily on virtual functions the
 *  threadInit() call (which ultimately starts the thread) must be the *last
 *  call in the derived class's constructor. Thus for derivations from
 *  VisusDataTransform by more than one step caution must be taken to
 *  not either initialize the thread twice or to access virtual
 *  functions too early.
 */
class VisusDataTransform : public VisusGroup, public VisusConsumer, public VisusProducer, 
                           public VisusSampleExtractor 
{
public:
  
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  

  //! Default constructor
  VisusDataTransform(VisusNodeType type, uint8_t number_of_inputs, VisusData* const product, 
                     VisusProcessingMode mode = VISUS_INCREMENTAL_PROCESSING);

  //! Destructor
  virtual ~VisusDataTransform();

  //! Return the current processing mode
  VisusProcessingMode processingMode() const {return mProcessingMode;}

  //! Set the current processing mode
  void processingMode(VisusProcessingMode mode) {mProcessingMode = mode;}

  //! Check for new new parameters
  /*! Derived classes must overload this call as the *only* place
   *  where internal parameters are synchronized with their shared
   *  counterparts. The function should return 1 if all parameters
   *  were up-to-date and valid, >1 if the parameter were not
   *  up-to-date but valid, and 0 otherwise.
   *
   *  @return 1 if all parameters are current and valid; >1 if the
   *  parameters are valid but were not up-to-date; 0 otherwise
   */ 
  virtual int checkParameters() = 0;

  //! Check the incoming data
  /*! Derived classes can re-implement this call to perform any checks
   *  on the input data to ensure that the incomping data is
   *  valid. The default implementation 1 to indicate all inputs are
   *  assumed to be valid. This function is called either with the
   *  mSink list of the VisusConsumer or with a list of pointers
   *  directly to the producers of the data. In both case the caller
   *  can assume the reference VisusData objects do not change during
   *  the call (either because its internal data only itself could
   *  change or because the appropriate mutexes have been
   *  locked). This call provides the ability, for example, to ensure
   *  that two hierarchically extracted data set have the same
   *  resolution. The result of this call should not depend on any
   *  parameters but only on the data itself.
   *  @param inputs: List of all input data 
   *  @return 1 if the input data is valid and consistent; 0 otherwise
   */
  virtual int checkData(const std::vector<VisusData*>& inputs);

  //! Call to process the data
  /*! Derived classes must overload this call to preform whatever
   *  processing is necessary. When this function is called all
   *  parameters necesssary for processing have been checked for
   *  validity (using updateParameters) and all input as well as the
   *  product data have been locked. The function is called with
   *  either the mSink list of the VisusConsumer or with a list of
   *  pointers directly to the producers of the data. While this
   *  requires some initial casting in the produce call it allows a
   *  trivial switch to a pieplined processing scheme in which
   *  consumers maintain no local copy of their inputs. 
   *  @return 1 if successful; 0 otherwise
   */
  virtual int process(const std::vector<VisusData*>& inputs) = 0;


  //! Call for threadless production
  /*! This call will perform a single round of production to allow for
   *  a threadless environment. 
   *  return 1 is successful; 0 otherwise
   */
  virtual int processThreadless();

  virtual VisusDataRequest latestRequest() const;

protected:
  
  //! The current processing mode
  VisusProcessingMode mProcessingMode;

  //! List of the last processed data ids of all inputs
  std::vector<VisusDataID> mDataID;

  //! List of pointers directly to the products
  std::vector<VisusData*> mStreamingInput;
  
  //! Overloaded display call also used for productiong in threadless mode
  virtual void display3D(VisusTransformation3D model_view_3D);

  //! The actual thread to transform the data
  virtual void* extractionLoop(void* );

  //! Check the input data incrementally
  int checkDataIncrementally();

  //! Check the data in streaming mode
  /*! This function checks the data by reading it directly from the
   *  producers. If the data is valid it will return with all the
   *  producers read-locked and the caller must ensure that the locks
   *  are released later. The locks are set because you do not want
   *  the data to change between checking and processing it.  
   *  @return 1 is the data is valid and unchanged; >1 if the data is
   *  valid but has changed; and 0 otherwise
   */ 
  int checkDataStreaming();
    
  //! This call unlocks all producers
  int unlockInputs();
};  

  

  



#endif

