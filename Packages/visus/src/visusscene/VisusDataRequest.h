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


#ifndef VISUSDATAREQUEST_H
#define VISUSDATAREQUEST_H

#include <vector>

#include "VisusTransformation3D.h"
#include "VisusBoundingBox.h"
#include "VisusOpenGLState.h"
#include "PvTimeState.h"

struct XMLNode;

enum VisusRequestType {
  
  VISUS_1D_REQUEST = 0,
  VISUS_2D_REQUEST = 1,
  VISUS_3D_REQUEST = 2,
};

/*! A data request encapsulates a certain query for raw data. The
 *  query is formulated in terms of the size of the region of
 *  interest, its transformation relative to the local coordinate
 *  system, and its start and end resolution. 
 */
class VisusDataRequest
{
public:

  static const char* XML_TAG;

  //! Default constructor 
  VisusDataRequest();

  //! Copy constructor
  VisusDataRequest(const VisusDataRequest& request);

  //! Destructor
  ~VisusDataRequest() {}

  //! Assignement operator
  VisusDataRequest& operator=(const VisusDataRequest& request);

  //! Comparisons between requests
  bool operator==(const VisusDataRequest& request); 

  //! Comparisons between requests
  bool operator!=(const VisusDataRequest& request) {return !((*this) == request);}

  //! Return a reference to the internal transformation matrix
  /*! Return a reference to the internal transformation matrix. This
   *  call is used to directly rotate/translate the matrix from the
   *  gui. Note that, the matrix should not be scaled as this will
   *  interfer/accumulate with the extent parameter and obscure the
   *  true size of the extracted data
   */
  VisusTransformation3D& transformation() {return mMatrix;}

  //! Return the current domain bounding box 
  VisusBoundingBox domainBBox() {return mDomainBBox;}

  //! Return the current extent as an array of length  
  /*! Return the current extent as an array of length for the
   *  different coordinate axis
   *  @return: The size of the requested region in 
   *           each coordinate axis 
   */
  std::vector<double> extent() const {return mExtent;}

  //! Return the axis-aligned bounding box of the query region
  /*! Return the axis-aligned bounding box of the query region. This
   *  is the bounding box of the rotated and translated extent sized
   *  box.
   *  @return The axis-aligned bounding box of the query region
   */
  VisusBoundingBox queryRegion() const;

  //! Return the lowest resolution acceptable for this request
  std::vector<int> startStrides() const {return mStartStrides;}

  //! Return the higest resolution necessary for this request
  std::vector<int> endStrides() const {return mEndStrides;}

  //! Return the strides of the i'th resolution
  /*! Return the number of strides for the i'th resolution counted
   *  from the lowest (mStartStrides) to the highest (mEndStrides). If
   *  i<0 a warning will be printed and mStartStrides will be
   *  return. If i>=numberOfResolutions() a warning will be printed
   *  and mEndStrides will be returned.
   *  @param i: the "index" of the resolution to be returned
   *  @return A set of strides defining the i'th lowest resolution 
   *          this request describes
   */
  std::vector<int> strides(int i) const;
  
  //! Return the number of different resolutions this request entails
  int numberOfResolutions() const {return mNumResolutions;}

  //! return the current time state
  PvTimeState timeState() const {return mTimeState;}

  //! Return the request type
  VisusRequestType requestType() const {return mRequestType;}
 
  //! Set the transformation matrix
  int transformation(const VisusTransformation3D& matrix);

  //! Set the domain bounding box
  void domainBBox(const VisusBoundingBox& bbox) {mDomainBBox = bbox;}

  //! Set the extent from an array of sizes
  void extent(const std::vector<double>& e) {mExtent = e;}
  
  //! Set starting strides
  void startStrides(const std::vector<int>& start) {setStrides(start,mEndStrides);}

  //! Set starting strides
  void endStrides(const std::vector<int>& end)  {setStrides(mStartStrides,end);}

  //! Set the resolution range requested
  int setStrides(const std::vector<int>& start, const std::vector<int>& end);
  
  //! Set the time state
  int timeState(const PvTimeState& time);
  
  //! Set the request type
  void requestType(VisusRequestType type) {mRequestType = type;}

  //! Determine whether the current request is valid
  /*! Determine whether the extents and strides are
   *  self-consistent. 
   */
  bool valid();

  //! Draw the bounding box of the requested piece of data
  void drawBoundingBox() const;

  //! Add the negative translation to the center of the box to the
  //! given transformation
  VisusTransformation3D translateToCenter() const;

  //! Translate the request
  void translate(const VisusTransformation3D& acc, const VisusOpenGLState& state, float x, float y);

  //! Translate the request orthogonal to its primary dimension
  void shift(const VisusTransformation3D& acc, const VisusOpenGLState& state, float x, float y);

  //! Build XML instance data from XML tree
  bool fromXML(XMLNode& node);

  //! Build XML instance data into XML tree
  void toXML(XMLNode& parent) const;

protected:

  //! The current transformation matrix
  /*! The current transformation matrix defining the orientation and
   *  location of the requested data. The location is assumed to be
   *  the \e center of the bounding box of this request.
   */
  VisusTransformation3D mMatrix;

  //! The domain bounding box of the source data
  VisusBoundingBox mDomainBBox;

  //! The in each dimension length of the requested region 
  std::vector<double> mExtent;

  //! The strides in each dimension for the lowest acceptable
  //! resolution
  /*! An array of strides that determine which resolution of the data
   *  is the lowest resolution acceptable for this query. The length
   *  of the array is smaller or equal the number of dimensions
   *  present in the data. If the length is smaller than the dimension
   *  the missing missing strides are assumed to be n[x|y|z]-1 meaning
   *  only one sample is extracted for this dimension. (E.g. you can
   *  specifiy a slice using a 2D strides array.)
   */
  std::vector<int> mStartStrides;

  //! The strides in each dimension for the higest resolution
  //! necessary for this query
  /*! An array of strides that determines the highest resolution that
   *  should be extracted for this query. The length of the array is
   *  smaller or equal the number of dimensions present in the
   *  data. If the length is smaller than the dimension the missing
   *  missing strides are assumed to be n[x|y|z]-1 meaning only one
   *  sample is extracted for this dimension. (E.g. you can specifiy a
   *  slice using a 2D strides array.)
   */
  std::vector<int> mEndStrides;

  //! Number of different resolutions for this request 
  int mNumResolutions;

  //! The time step at which to extract the data
  PvTimeState mTimeState;

  //! The request type 
  VisusRequestType mRequestType;

  //! Compute the number of resolutions for this request
  int determineNumResolutions() const;
};

#endif
