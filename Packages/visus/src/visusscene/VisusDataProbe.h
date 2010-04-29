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


#ifndef VISUSDATAPROBE_H
#define VISUSDATAPROBE_H

#include <vector>

#include "VisusDataSource.h"
#include "VisusGroup.h"
#include "VisusSmartPointer.h"
#include "VisusText.h"
#include "VisusDataDescription.h"
#include "VisusFieldIndex.h"
#include "VisusReadWriteLock.h"
#include "VisusGlobalTime.h"

class VisusDataProbe;
typedef VisusSmartPointer<VisusDataProbe> pVisusDataProbe;

class VisusDataProbe : public VisusGroup
{
public:

  static pVisusDataProbe instantiate();
  
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  

  //! Default constructor
  VisusDataProbe();
  
  //! Destructor
  virtual ~VisusDataProbe();

  /***************************************************************
   ******         Access Functions                       *********
   **************************************************************/  

  //! The data probe should not be able to rotate 
  virtual void rotate(float x, float y) {return;}

  //! The Data proble should not be able to scale
  virtual void scale(float x, float y) {return;}

  //! Translate the query point
  void translate(float x, float y);

protected:

  virtual void display3D(VisusTransformation3D model_view_3D = VisusTransformation3D());

  virtual void display2D(VisusTransformation2D model_view_2D = VisusTransformation2D());

private:

  //! The current data describtor
  VisusDataDescription mDataset;

  //! The current field index
  VisusFieldIndex mFieldIndex;

  //! Pointer to a data source
  VisusDataSource* mDataSource;

  //! Flag to indicate whether or not to render the info
  bool mRenderInfo;
  
  //! The x,y,z indices of the current sample
  std::vector<int> mIndex;

  //! The x, y, z position of the current sample
  std::vector<double> mPosition;

  //! Pointer to the last data value 
  unsigned char* mData;

  //! The info text used to render the info
  VisusText mInfo;

  //! Current time
  VisusGlobalTime mCurrentTime;

  //! Synchronize the data set and info text with the shared values
  void synchronize(bool must_update = false);

  //! Compute the new info text
  int createInfoText(std::vector<int> index,PvDataType type);

  //! Read-write lock to protect the mDataSource pointer
  VisusReadWriteLock mDataLock;

};




#endif
