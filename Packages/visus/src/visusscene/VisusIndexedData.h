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


#ifndef VISUSINDEXEDDATA_H
#define VISUSINDEXEDDATA_H

#include <vector>

#include "VisusStdInt.h"
#include "VisusBoundingBox.h"
#include "VisusMetricData.h"


class VisusIndexedData : public VisusMetricData
{
public:

  //! Typedef for the vertex attributes
  typedef float VertexDataType; 

  //! Typedef for the index attirbutes
  typedef int32_t IndexDataType;

  //! Default constructor
  VisusIndexedData(unsigned char vertex_dim=3,VisusDataFormat format=VISUS_INDEXED_DATA);
  
  //! Copy constructor
  explicit VisusIndexedData(const VisusIndexedData& data);

  //! Default Destructor
  ~VisusIndexedData();

  //! Return the dimension of a vertex
  int vertexDim() const {return mVertexDim;}

  //! Return the number of spatial dimensions
  int spatialDim() const {return mSpatialDim;}

  //! Set the number of spatial dimensions
  void spatialDim(uint8_t dim) {mSpatialDim = dim;}

  //! Return the current number of vertices
  uint32_t nrOfVertices() const {return mVertices->size();}

  //! Return the current number of elements
  uint32_t nrOfElements() const {return mElements->size();}

  //! Return a reference to vertex data
  std::vector<VertexDataType>& vertex(IndexDataType i);

  //! Return a const reference to vertex data
  const std::vector<VertexDataType>& vertex(IndexDataType i) const;
  
  //! Return a pointer to the vertex data
  /*! Return a const pointer to the vertex data. This function should
   *  be used sparingly and is mainly designed to facilitate
   *  rendering. Getting a float pointer allows calling glVertex3f for
   *  example. 
   *  @param i: index of the vertex of interest
   *  @return constant pointer to the vertex data of the i'th vertex
   */
  const VertexDataType* vertexAddress(IndexDataType i) const;
 
  //! Return a pointer to a specific component of the vertex data
  const VertexDataType* componentAddress(IndexDataType i, int comp) const;

  //! Return a reference to the index data
  std::vector<IndexDataType>& element(IndexDataType i); 

  //! Return a const reference to the index data
  const std::vector<IndexDataType>& element(IndexDataType i) const; 

  //! Return the bounding box
  const VisusBoundingBox& boundingBox() const {return mBBox;}

  //! Recompute the vertex normals
  void computeVertexNormals();
  
  /***************************************************************
   ******         Memory Managment                       *********
   **************************************************************/
  
  //! Reserve space for n vertices 
  int reserveVertices(int n);

  //! Reserve space for n elements 
  virtual int reserveElements(int n);

  //! Add a single vertex to the end of the list
  int addVertex(const std::vector<VertexDataType>& v);

  //! Add a single element to the end of the list
  int addElement(const std::vector<IndexDataType>& e);

  /***************************************************************
   ******         Data Flow Functionality                *********
   **************************************************************/

  //!Swap the content of \e this with the content of data. 
  virtual int swapContent(VisusData* data);

  //! Copy the content of data into \e this
  virtual int copyContent(const VisusData* data);

  //! Determine whether \e this can load its contents from data
  virtual bool readCompatible(const VisusData* data) const;

  /***************************************************************
   ******         Data I/O                               *********
   **************************************************************/

  //! Add the element described by the given list of vertices
  virtual int constructElement(const std::vector<std::vector<VertexDataType> >& vert); 

  //! Add the element described by the given list of vertices. 
  virtual int constructElement(const std::vector<float>& vertices);

  //! Add all elements described by the given list of vertices
  virtual int constructElements(const std::vector<std::vector<std::vector<VertexDataType> > >& lines); 
  

protected:

  void toXMLLocalVariables(XMLNode& parent) const;
  bool fromXMLLocalVariables(XMLNode& node, XMLDataStorage storageType);

  template <typename T>
  void addText(XMLNode& node, const std::vector<std::vector<T > >* items) const;

  template <typename T>
  bool getText(XMLNode& node, std::vector<std::vector<T > >* items, XMLDataStorage storageType);

   //! The number of VertexDataTypes stored for each vertex
  uint8_t mVertexDim;

  //! The number of spatial coordinates given for each vertex
  uint8_t mSpatialDim;

  //! The vertex array 
  std::vector<std::vector<VertexDataType> >* mVertices;

  //! The element array
  std::vector<std::vector<IndexDataType> >* mElements;

  //! The bounding box of the mesh
  VisusBoundingBox mBBox;

  std::vector<VertexDataType> weightedNormal(const std::vector<VertexDataType>& u,
                                             const std::vector<VertexDataType>& v,
                                             const std::vector<VertexDataType>& w) const;
    
  //! Compute the normal of an edge 
  std::vector<VertexDataType> edgeNormal(const std::vector<VertexDataType>& u,
                                         const std::vector<VertexDataType>& v) const;
  //! Compute the angle around v
  VertexDataType interiorAngle(const std::vector<VertexDataType>& u,
                               const std::vector<VertexDataType>& v,
                               const std::vector<VertexDataType>& w) const;
};

#endif
