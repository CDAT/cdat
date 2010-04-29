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


#ifndef VISUSBLOCKDATA_H
#define VISUSBLOCKDATA_H

#include <vector>

#include "VisusMetricData.h"
#include "PvDataType.h"
#include "VisusBoundingBox.h"

class VisusBlockData;


//! Rectangular block/slice/line of data
/*! Rectangular block/slice/line of data together with a bitmask
 *  indicating whether the corresponding sample is valid (1) or
 *  invalid (0). VisusBlockData can be used in two ways. First as a
 *  self-managed adaptive array: By setting the relevant parameter
 *  (i.e. sample size and samples) and calling reserve()
 *  VisusBlockData will automatically (re-)allocate the data and mask
 *  arrays to be large enough to handle the current data set and the
 *  user can, for example, use memcpy to update/copy data. In this
 *  case the m[Data|Mask]Size parameter will contain the current
 *  capacity of the arrays and the destructor will free the allocated
 *  memory. The second option is leave mDataSize and mMaskSize at 0
 *  and set the mData/mMask pointers directly. This allows the user to
 *  control the memory allocation more directly. In particular it
 *  allows to use buffers allocated somewhere else without copying the
 *  data. Naturally, this use is somewhat more dangerous as the user
 *  is now responsible for these buffers to have adequate size and to
 *  de-/allocate them appropriately. If an instance uses such
 *  "outside" buffers it is said to not-own the buffers. In particular
 *  such a block data cannot swap its content . 
 * 
 *  While the second use case seems overly complicated and error-prone
 *  it is nevertheless necessary, for example, during data
 *  extraction. Working with outside buffers enables close ties to
 *  third-party interfaces (i.e. the idx library or python) without
 *  the need for a common data structure or data replication.
 */
class VisusBlockData : public VisusMetricData
{
public:

  //! Masks used for the bit mask computation
  static const uint8_t sBitMask[8];

  //! Default constructor
  VisusBlockData();
  
  //! Default destructor
  ~VisusBlockData();

  //! Return the cell center adjustment matrix
  const VisusTransformation3D& cellCentered() const {return mCellCentered;}

  //! Return the current data type
  PvDataType dataType() const {return mDataType;}

  //! Return the number of samples for each dimension
  std::vector<int> samples() const {return mSamples;}
  
  //! Return the bounding box in domain space
  void getDomainBoundingBox(std::vector<double>& left_lower, std::vector<double>& right_upper) const; 

  //! Return the bounding box in domain space
  void getDomainBoundingBox(VisusBoundingBox& bbox) const; 

  //! Return the sample size
  int sampleSize() const {return mSampleSize;}

  //! Return the min and max bounds of the field
  void getFieldRange(double &min_f, double &max_f) const;

  //! Return the data size
  int dataSize() const {return mDataSize;}

  //! Return the mask size
  int maskSize() const {return mMaskSize;}

  //! Return a pointer to the internal data array
  unsigned char *data() {return mData;}

  //! Return a const pointer to the internal data array
  const unsigned char *data() const {return mData;}
  
  //! Return a pointer to the internal mask array
  unsigned char *getMask() {return mMask;}

  //! Return a const pointer to the internal mask array
  const unsigned char *getMask() const {return mMask;}

  //! Return a single "bit" of the mask as 0|1 char
  unsigned char mask2(int i) const {return ((mMask[i >> 3] & sBitMask[i%8]) != 0);}
  
  //! Return the size of the bounding box
  std::vector<double> extent() const {return mExtent;}

  //! Return the negative translation to the center of the extent box
  VisusTransformation3D translateToCenter() const;

  //! Set the cell center adjustment matrix
  void cellCentered(const VisusTransformation3D& m) {mCellCentered = m;}

  //! Set the data size
  void dataSize(int size) {mDataSize = size;}

  //! Set the internal array
  void data(unsigned char* buffer) {mData = buffer;}

  //! Set the mask size
  void maskSize(int size) {mMaskSize = size;}

  //! Set the internal mask array 
  /*! Set the internal mask array. The function cannot be overloaded
   *  in the ususal style as otherwise the mask(int) and mask(unsigned
   *  char*) have ambivalent types
   */
  void setMask(unsigned char* m) {mMask = m;}

  //! Set a single bit of the mask array
  void mask(int i, bool bit); 

  //! Set the data type
  void dataType(PvDataType type);

  //! Set the number of samples
  void samples(const std::vector<int>& s) {mSamples = s;}

  //! Set the sample size
  void sampleSize(int size) {mSampleSize = size;}

  //! Set the domain bounding box
  void setDomainBoundingBox(const std::vector<double>& left_lower, 
                            const std::vector<double>& right_upper);

  //! Set the domain bounding box
  void setDomainBoundingBox(const VisusBoundingBox& bbox);

  //! Set the bounding box
  void extent(const std::vector<double>& e) {mExtent = e;}
    
  //! Set the range of function values of the field
  void setFieldRange(double min_f, double max_f);

  //! Compactify the dimensions in case of lower dimensional data
  /*! This function assumes that the data is supposed to be of the
   *  given dimension and will adjust the data accordingly. For
   *  example, a plane with [1,10,10] many samples will be changed to
   *  [10,10,1] samples and all the matrices will be adjusted
   *  accordingly. The assumption is that the matrices are pure
   *  scaling and translation matrices and "empty" dimensions will
   *  contain exactly one sample. 
   *  @param dim: the target dimension of the data
   *  @result 1 if successful | 0 otherwise
   */
  int compactDimensions(int dim);

  //! Exchange the content of \e this with data
  virtual int swapContent(VisusData* data);

  //! Copy the content of \e this with data
  virtual int copyContent(const VisusData* data);

  //! Determine whether \e this can load it contents from data
  virtual bool readCompatible(const VisusData* data) const;
    
  //! For the current set of dimensions, samples number, data type,
  //  etc. allocate sufficient memory
  virtual int reserveSpace();

  
protected:
  void toXMLLocalVariables(XMLNode& parent) const;
  virtual void saveXMLData(XMLNode& node) const;

  bool fromXMLLocalVariables(XMLNode& parent, XMLDataStorage storageType);
  virtual bool loadXMLData(XMLNode& node, XMLDataStorage storageType);

  //! Matrix to adjust for cell centered data
  /*! This matrix stores the adjustments necessary to go from node
   *  centered to cell centered data. It is computed such that
   *  mMatrix*mCellCentered results in a transformation matrix that
   *  draws the data in cell centered fashion.
   */
  VisusTransformation3D mCellCentered;

  //! Data array of size >= mSampleSize*mSamples[0]*mSamples[1]*mSamples[2]
  unsigned char *mData;
  
  //! Data type of the mData array
  PvDataType mDataType;

  //! Sample size necessary for mDataType = PV_RAW
  int mSampleSize;

  //! Pointer to the masking array of size >= mSamples[0]*mSamples[1]*mSamples[2]
  unsigned char *mMask;

  //! Size of the bounding box
  std::vector<double> mExtent;
  
  //! Number of samples in each dimension
  std::vector<int> mSamples;
  
  //! Minimal field value
  double mMinFieldValue;
  
  //! Maximal field value
  double mMaxFieldValue;

  /************************************
   * Internal memory managment        *
   ***********************************/
  
  //! Current size of the mData array in bytes
  int mDataSize;

  //! Current capacity of the mData array in bytes
  int mDataCapacity;

  //! Current size of the mMask array in bytes
  int mMaskSize;

  //! Current capacity of the mMask arrar in bytes
  int mMaskCapacity;

  //! Return the number of samples used for the current set of
  //! dimensions and sample numbers. 
  int usedSamples();

  //! Swap the X and Y dimensions
  void swapDimensionXY();

  //! Swap the Y and Z dimensions
  void swapDimensionYZ();
};

#endif
