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


#ifndef VISUSISOSURFACE_H
#define VISUSISOSURFACE_H

#include "VisusDataTransform.h"
#include "VisusSmartPointer.h"
#include "VisusBlockData.h"
#include "VisusMeshData.h"
#include "VisusTransformation3D.h"

class VisusIsoSurface;
typedef VisusSmartPointer<VisusIsoSurface> pVisusIsoSurface;

// This is necessary On Windows (struct vs class) otherwise get unresolved externals
typedef struct ConDataset;
typedef struct Contour3dData;

//! Node which extracts an isosurface
/*! VisusIsoSurface implements a node which extracts an isosurface
 *  from a rectangular piece of volume data. The node is both a
 *  consumer getting its data from a data extractor and a producer
 *  creating a mesh data structure of the resulting iso-surface.
 */
class VisusIsoSurface : public VisusDataTransform
{
public:

  static pVisusIsoSurface instantiate();
  
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  

  //! Default constructor
  VisusIsoSurface();

  //! Destructor
  virtual ~VisusIsoSurface();

  
  /***************************************************************
   ******           Access Functions                     *********
   **************************************************************/  

  //! Return the current translation style
  VisusTranslationStyle translationStyle() const;

  //! Set the translation style
  void translationStyle(VisusTranslationStyle style);


  //! Define the rotation function as no-op 
  virtual void rotate(float x, float y) {}
  
  //! Define the translation function as translating the request
  virtual void translate(float x, float y) {translateRequest(x,y);}

  //! Translate the request normal to the current slice
  void translateRequest(float x, float y);

  //! Attach a data extractor as input
  virtual int connectIso(pVisusProducer producer);
  
  /***************************************************************
   ******           DataTranformation Interface          *********
   **************************************************************/  

  //! Check for new parameters
  virtual int checkParameters();

  //! Check the incoming data
  virtual int checkData(const std::vector<VisusData*>& inputs);

  //! Call to process the data
  virtual int process(const std::vector<VisusData*>& inputs);

  //! Display function for the bounding box 
  virtual void displayBoundingBox() const; 

  /***************************************************************
   ******       Private Variables And Functions          *********
   **************************************************************/  

protected:

  void toXMLLocalVariables(XMLNode& node);
  bool fromXMLLocalVariables(XMLNode& node);

  //! Constructor to allow derivers to set number of consumers
  VisusIsoSurface(VisusNodeType type, int numConsumers, VisusProcessingMode mode);

  //! The local copy of the block of data to extract the isosurface from
  VisusBlockData mSourceData;

  //! The libcontout data set
  ConDataset* mContourData;

  //! The resulting isosurface as mesh data
  VisusMeshData mIsoSurface;

  //! The last used iso-value
  VisusIsoValue mIsoValue;
 
   //! Get the contour data from the libcontour library
  virtual Contour3dData* getContourData(ConDataset* contourData, const VisusIsoValue& isoValue);

  //! Return the libcontour contour type for the given PVDataType
  int getContourType(const PvDataType type, const int numData);

};

 

#endif
