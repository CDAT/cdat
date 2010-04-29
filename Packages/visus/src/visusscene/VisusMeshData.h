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


#ifndef VISUSMESHDATA_H
#define VISUSMESHDATA_H

#include <vector>

#include "VisusData.h"
#include "VisusIndexedData.h"
#include "VisusBoundingBox.h"

//! Data type to encode any indexed mesh format without connectivity
/*! VisusMeshData is a container able to store an indexed mesh of any
 *  dimension.  The storage consists of a float array storing
 *  per-vertex data and an int-array storing list of indices. A 3D
 *  triangle mesh for example is stored using a 3-by-n vertex array
 *  and a 3-by-m index array.
 */
class VisusMeshData : public VisusIndexedData
{

public:
  
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/
  
  //! Default constructor 
  /*! The constructor is the only way to set the vertex and element
   *  dimension.
   *  @param vertex_dim: number of VertexDataTypes stored for each
   *  vertex @paran element_dim: number of indexDataTypes stored for
   *  each element
   */
  VisusMeshData(uint8_t vertex_dim=3, uint8_t element_dim=3);

  //! Copy constructor
  explicit VisusMeshData(const VisusMeshData& mesh);

  //! Destructor
  virtual ~VisusMeshData();

  /***************************************************************
   ******         Access Functions                       *********
   **************************************************************/
  
  //! Return the dimension of an element
  int elementDim() const {return mElementDim;}

  const VertexDataType* elementNormalAddress(IndexDataType i) const;
  
  /***************************************************************
   ******         Memory Managment                       *********
   **************************************************************/
  
  //! Reserve space for n elements 
  virtual int reserveElements(int n);

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
   ******         File Interface                         *********
   **************************************************************/
  
  //! Load a mesh from an obj file
  int loadObj(const char* filename);

protected:
  
  void toXMLLocalVariables(XMLNode& parent) const;
  bool fromXMLLocalVariables(XMLNode& node, XMLDataStorage storageType);

  //! The number of ElementDataTypes stored for each element
  uint8_t mElementDim;

  //! The element wise normal array 
  std::vector<std::vector<VertexDataType> >* mNormals;
};
  
#endif


