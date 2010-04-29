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


#ifndef VISUSMETRICDATA_H
#define VISUSMETRICDATA_H

#include <vector>

#include "VisusData.h"
#include "VisusUnits.h"

class VisusMetricData : public VisusData
{
public:
  
  //! Default constructor
  VisusMetricData(VisusDataFormat t = VISUS_UNDEFINED_DATA);

  //! Default Destructor
  ~VisusMetricData();

  //! Return the transformation matrix
  const VisusTransformation3D& matrix() const {return mMatrix;}
  
  //! Return the physical unit of the domain 
  VisusUnit unit() const {return mUnit;}

  //! Return the bounding box in domain space
  virtual void getDomainBoundingBox(std::vector<double>& left_lower, std::vector<double>& right_upper) const; 

  //! Return the bounding box in domain space
  virtual void getDomainBoundingBox(VisusBoundingBox& bbox) const; 

  //! Set the transformation matrix
  void matrix(const VisusTransformation3D& m) {mMatrix = m;}

  //! Set the physical unit of the domain
  void unit(VisusUnit u) {mUnit = u;}

  //! Set the domain bounding box
  virtual void setDomainBoundingBox(const std::vector<double>& left_lower, 
                                    const std::vector<double>& right_upper);

  //! Set the domain bounding box
  virtual void setDomainBoundingBox(const VisusBoundingBox& bbox);

  //! Exchange the content of \e this with data
  virtual int swapContent(VisusData* data);

  //! Copy the content of \e this with data
  virtual int copyContent(const VisusData* data);


protected:
  void localXMLVariables(XMLNode& parent) const;
  bool fromLocalXMLVariables(XMLNode& node);

  //! Tranformation matrix 
  /*! The local transformation matrix is used to adjust the "natural"
   *  coordinates of the local index space to the domain coordinate system of
   *  the data. For example, for a given data request of box
   *  [-1,-1]x[2,2] a data extractor might return the box [0,0]x[2,2]
   *  as the rest falls outside the bounding box of the data set (the
   *  idx extractor for example). The display node however must draw
   *  the data wrt. the request it issued to preserve interactivity
   *  (if necessary the display will draw "old" data at the location
   *  of the new request see, for example, VisusOrthogonalSlice). To
   *  adjust the difference in the two coordinate systems
   *  VisusBlockData stores this additional transformation matrix.
   */
  VisusTransformation3D mMatrix;

  //! The unit of the underlying domain
  VisusUnit mUnit;

  //! The left lower corner of the domain bounding box in unit coordinates
  std::vector<double> mLeftLower;
  
  //! The right upper corner of the domain bounding box in unit coordinates
  std::vector<double> mRightUpper;
  
};


#endif
