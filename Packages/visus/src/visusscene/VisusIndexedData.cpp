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

#include "xmlParser.h"

#include "VisusAssert.h"
#include "VisusIndexedData.h"
#include "VisusDefinitions.h"
#include "VisusXMLInterface.h"


VisusIndexedData::VisusIndexedData(unsigned char vertex_dim, VisusDataFormat format)
  : VisusMetricData(format), mVertexDim(vertex_dim), mSpatialDim(MIN(vertex_dim,3))
{
  if (mVertexDim == 0) {
    vwarning("VisusIndexedData constructed with 0-dimensional vertices.");
  }
  mVertices = new std::vector<std::vector<VertexDataType> >;
  mElements = new std::vector<std::vector<IndexDataType> >;
  
}

VisusIndexedData::VisusIndexedData(const VisusIndexedData& data) 
  : VisusMetricData(data), mVertexDim(data.mVertexDim) 
{
  if (mVertexDim == 0) {
    vwarning("VisusIndexedData constructed with 0-dimensional vertices.");
  }
  mVertices = new std::vector<std::vector<VertexDataType> >;

  *mVertices = *(data.mVertices);
  *mElements = *(data.mElements);
  mBBox = data.mBBox;
}


VisusIndexedData::~VisusIndexedData()
{
  delete mVertices;
  delete mElements;
}

std::vector<VisusIndexedData::VertexDataType>& VisusIndexedData::vertex(IndexDataType i)
{
  if ((i < 0) || (i >= (int)nrOfVertices())) {
    vwarning("Index out of range: no such vertex. Return first vertex");

    // We need to return something in case of a failure. We return hte
    // first vertex and if there is no such vertex we create one
    if (nrOfVertices() == 0) 
      reserveVertices(1);

    return (*mVertices)[0];
  }


  return (*mVertices)[i];
}

const std::vector<VisusIndexedData::VertexDataType>& VisusIndexedData::vertex(IndexDataType i) const
{
  if ((i < 0) || (i >= (int)nrOfVertices())) {
    vwarning("Index out of range: no such vertex. Return first vertex");

    // This is a const function we cannot construct a
    // vertex. Therefore, if there is no vertex reference we could return we assert
    if (nrOfVertices() == 0) {
      vwarning("Illegal acces to vertex array. Cannot recover.");
      assert(false);
    }

    return (*mVertices)[0];
  }


  return (*mVertices)[i];
}

const VisusIndexedData::VertexDataType* VisusIndexedData::vertexAddress(IndexDataType i) const
{
  return componentAddress(i,0);
}

const VisusIndexedData::VertexDataType* VisusIndexedData::componentAddress(IndexDataType i, int comp) const
{
  if ((i < 0) || (i >= (int)nrOfVertices())) {
    vwarning("Index out of range: no such vertex. Return first component of first vertex");
    
    // This is a const function we cannot construct a
    // vertex. Therefore, if there is no vertex reference we could return we assert
    if (nrOfVertices() == 0) {
      vwarning("Illegal acces to vertex array. Cannot recover.");
      return NULL;
    }

    return &((*mVertices)[0][0]);
  }


  if ((comp < 0) || (comp >= vertexDim())) {
    vwarning("Component index out of range no such component.");
    return &((*mVertices)[i][0]);
  }

  return &((*mVertices)[i][comp]);
}

std::vector<VisusIndexedData::IndexDataType>& VisusIndexedData::element(IndexDataType i)
{
  if ((i < 0) || (i >= (int)nrOfElements())) {
    vwarning("Index out of range: no such element. Returning first element.");
    
    // We need to return something in case of a failure. We return the
    // first element and if there is no auch element we create one
    if (nrOfElements() == 0) 
      reserveElements(1);

    return (*mElements)[0];
  }

return (*mElements)[i];
}

const std::vector<VisusIndexedData::IndexDataType>& VisusIndexedData::element(IndexDataType i) const 
{
  if ((i < 0) || (i >= (int)nrOfElements())) {
    vwarning("Index out of range: no such element. Returning first element.");
    
    // This is a const function we cannot construct an
    // element. Therefore, if there is no element we can reference we
    // assert
    if (nrOfElements() == 0) {
      vwarning("Illegal access to element array. Cannot recover.");
      assert(false);
    }

    return (*mElements)[0];
  }

  return (*mElements)[i];
}

int VisusIndexedData::reserveVertices(int n)
{
  try {
    mVertices->resize(n,std::vector<VertexDataType>(mVertexDim,0));
  }
  catch (const std::bad_alloc& /*error*/) {
    vwarning("Could not allocate enough memory for vertices.");
    return 0;
  }

  return 1;
}

int VisusIndexedData::reserveElements(int n)
{
  try {
    mElements->resize(n);
  }
  catch (const std::bad_alloc& /*error*/) {
    vwarning("Could not allocate enough memory for elements.");
    return 0;
  }

  return 1;
}

int VisusIndexedData::addVertex(const std::vector<VertexDataType>& v)
{
  try {
    mVertices->push_back(v);
  }
  catch (const std::bad_alloc& /*error*/) {
    vwarning("Could not allocate enough memory for new vertex.");
    return 0;
  }
    
  return 1;
}

int VisusIndexedData::addElement(const std::vector<IndexDataType>& e)
{
  try {
    mElements->push_back(e);
  }
  catch (const std::bad_alloc& /*error*/) {
    vwarning("Could not allocate enough memory for new element.");
    return 0;
  }
    
  return 1;
}

int VisusIndexedData::swapContent(VisusData* data) 
{
  // If I can't read the input I cannot proceed
  if (!readCompatible(data)) {
    vwarning("Data types not compatible cannot swap.");
    return 0;
  }
  
  // If I can read the input but the input cannot read from me I
  // default to copying the dat
  if (!data->readCompatible(this)) 
    return copyContent(data);

  
  VisusMetricData::swapContent(data);

  VisusIndexedData* mesh = (VisusIndexedData*)data;

  std::swap(mVertices,mesh->mVertices);
  std::swap(mElements,mesh->mElements);
  
  mVertexDim = mesh->mVertexDim;
  mSpatialDim = mesh->mSpatialDim;
  mBBox = mesh->mBBox;
  

  return 1;
}


int VisusIndexedData::copyContent(const VisusData* data)
{
  if (!readCompatible(data)) {
    vwarning("Data types not compatible cannot copy.");
    return 0;
  }

  VisusMetricData::copyContent(data);

  VisusIndexedData* mesh = (VisusIndexedData*)data;

  *mVertices = *(mesh->mVertices);
  *mElements = *(mesh->mElements);

  mBBox = mesh->mBBox;

  mVertexDim = mesh->vertexDim();
  mSpatialDim = mesh->mSpatialDim;

  return 1;
}
  
bool VisusIndexedData::readCompatible(const VisusData* data) const
{
  if (data == NULL)
    return false;
  
  if (data->dataCategory() == VISUS_INDEXED_SET)
    return true;
  
  return false;
}

int VisusIndexedData::constructElement(const std::vector<std::vector<VertexDataType> >& vertices)
{
  if (vertices[0].size() != mVertexDim) {
    vwarning("Number of vertex dimensions does not match. Proceed at your own risk.");
  }

  mVertices->insert(mVertices->end(),vertices.begin(),vertices.end());

  std::vector<IndexDataType> element(vertices.size(),0);
  std::vector<IndexDataType>::iterator it;


  for (it=element.begin();it!=element.end();it++) 
    *it = mVertices->size() - (element.end() - it);

  mElements->push_back(element);

  return 1;
}

int VisusIndexedData::constructElement(const std::vector<float>& vertices)
{
  if ((vertices.size() / mVertexDim) != (vertices.size() / ((float)mVertexDim))) {
    vwarning("Number of input values does not match with the given vertex dimension.");
    return 0;
  }

  std::vector<float>::const_iterator cit;

  for (cit=vertices.begin();cit!=vertices.end();cit+=mVertexDim) {
    mVertices->push_back(std::vector<VertexDataType>(cit,cit+mVertexDim));
  }

  
  std::vector<IndexDataType> element(vertices.size()/mVertexDim,0);
  std::vector<IndexDataType>::iterator it;


  for (it=element.begin();it!=element.end();it++) 
    *it = mVertices->size() - (element.end() - it);

  mElements->push_back(element);

  return 1;
}

int VisusIndexedData::constructElements(const std::vector<std::vector<std::vector<VertexDataType> > >& lines)
{
  uint32_t i;

  for (i=0;i<lines.size();i++) {
    
    if (constructElement(lines[i]) == 0) {
      vwarning("Could not add element.");
      return 0;
    }
  }

  return 1;
}
 
template <typename T>
void VisusIndexedData::addText(XMLNode& node, const std::vector<std::vector<T > >* items) const
{
  // Count max number of items
  int maxitems = 0;
  for (typename std::vector<std::vector<T> >::const_iterator iiter=items->begin(); 
       iiter!=items->end(); ++iiter) 
      maxitems += iiter->size() + 1;

  // Construct contiguous buffer
  const int bufsize = sizeof(T) * maxitems;
  unsigned char* buffer = new unsigned char[bufsize+1];

  node.addAttribute("numItems", (long) items->size());
  node.addAttribute("bufsize", bufsize);

  int position = 0;
  // Copy non-contiguous data into contiguous buffer
  for (typename std::vector<std::vector<T> >::const_iterator iiter=items->begin(); 
       iiter!=items->end(); ++iiter) 
  {
    // Save Num Items
    T value = items->size();
    memcpy(&buffer[position], &value, sizeof(T));
    position += sizeof(T);

    // Load Vector's Vector of items into buffer
    for (typename std::vector<T>::const_iterator idtIter=iiter->begin(); 
       idtIter!=iiter->end(); ++idtIter) 
    {
       vassert(position < bufsize);
       memcpy(&buffer[position], &(*idtIter), sizeof(T));
       position += sizeof(T);
    }
  }
  vassert(position == bufsize);

  // Save Buffer out to XML
  switch (VisusXMLInterface::sWriteXMLDataStorage)
  {
    case BASE64:
    {
      // Save data as BASE64
      XMLParserBase64Tool base64;
      XMLSTR encoded = base64.encode(buffer, bufsize);
      node.addText(encoded);
    }
    break;
    case EXTERNAL_FILE:
    {
      vwarning("saving data to external file is not yet supported");
    }
    break;
    case ASCII:
    {
      vwarning("saving data to external file is not yet supported");
    }
    break;
  }

  delete [] buffer;
}

template <typename T>
bool VisusIndexedData::getText(XMLNode& node, std::vector<std::vector<T > >* items, XMLDataStorage storageType)
{
  int numItems = xmltoi(node.getAttribute("numItems"));
  int bufsize  = xmltoi(node.getAttribute("bufsize"));
  unsigned char* buffer = new unsigned char[bufsize+1];

  switch (storageType)
  {
    case BASE64:
    {
      // Retrieve data as BASE64
      XMLParserBase64Tool base64;
      base64.decode(node.getText(), buffer, bufsize);
    }
    break;
    case EXTERNAL_FILE:
    {
      vwarning("saving data to external file is not yet supported");
      return false;
    }
    break;
    case ASCII:
    {
      vwarning("saving data to external file is not yet supported");
      return false;
    }
    break;
  }

  // Ensure vector is appropriate size
  if ((int)items->capacity() < numItems)
    items->resize(numItems);

  // Copy from contiguous buffer into non-contiguous data
  int position = 0;
  for (typename std::vector<std::vector<T> >::iterator iiter=items->begin(); 
       iiter!=items->end(); ++iiter) 
  {
    T value;
    memcpy(&value, &buffer[position], sizeof(T));
    position += sizeof(T);

    // Ensure item that is vector is appropriate size
    if ((int)iiter->capacity() < (long) value)
      iiter->resize((long)value);

    // Load up item that is vector
    for (typename std::vector<T>::iterator idtIter=iiter->begin(); 
       idtIter!=iiter->end(); ++idtIter) 
    {
       vassert(position < bufsize);
       memcpy(&(*idtIter), &buffer[position], sizeof(T));
       position += sizeof(T);
    }
  }
  vassert(position == bufsize);

  delete [] buffer;

  return true;
}


void VisusIndexedData::toXMLLocalVariables(XMLNode& parent) const
{
  VisusMetricData::localXMLVariables(parent);

  parent.addAttribute("vertexDim", mVertexDim);
  parent.addAttribute("spatialDim", mSpatialDim);

  // Save Vertices To XML tree
  XMLNode vertex = parent.addChild("vertices");
  XMLNode index  = parent.addChild("indices");

  switch (VisusXMLInterface::sWriteXMLDataStorage)
  {
    case ASCII:
    {
      std::stringstream ssVert;;
      for (std::vector<std::vector<VertexDataType> >::const_iterator viter=mVertices->begin(); 
           viter!=mVertices->end(); ++viter) 
      {    
        for (std::vector<VertexDataType>::const_iterator vdtIter=viter->begin(); 
             vdtIter!=viter->end(); ++vdtIter) 
        {
          ssVert << (*vdtIter) << " ";
        }
      }
      vertex.addText(ssVert.str().c_str());

      // Save Indices To XML tree
      std::stringstream ssIndex;;  
      for (std::vector<std::vector<IndexDataType> >::const_iterator iiter=mElements->begin(); 
           iiter!=mElements->end(); ++iiter) 
      {
        for (std::vector<IndexDataType>::const_iterator idtIter=iiter->begin(); 
             idtIter!=iiter->end(); ++idtIter) 
        {
          ssIndex << (*idtIter) << " ";
        }
      }
      index.addText(ssIndex.str().c_str());
    }
    break;
    default:
    {
      // Save Vertices
      addText(vertex, mVertices);

      // Save Elements
      addText(index, mElements);
    }
    break;
  }

  // Save BBox To Tree
  mBBox.toXML(parent);
}  


bool VisusIndexedData::fromXMLLocalVariables(XMLNode& node, XMLDataStorage storageType)
{
  if (! VisusMetricData::fromLocalXMLVariables(node))
    return false;

  mVertexDim = xmltoi(node.getAttribute("vertexDim"), mVertexDim);
  mSpatialDim= xmltoi(node.getAttribute("spatialDim"), mSpatialDim);

  // Save Vertices To XML tree
  XMLNode vertex = node.getChildNode("vertices");
  XMLNode index  = node.getChildNode("indices");

  switch (storageType)
  {
    case ASCII:
    {
      std::stringstream ssVert;;
      ssVert << vertex.getText();
      for (std::vector<std::vector<VertexDataType> >::iterator viter=mVertices->begin(); 
           viter!=mVertices->end(); ++viter) 
      {    
        for (std::vector<VertexDataType>::iterator vdtIter=viter->begin(); 
             vdtIter!=viter->end(); ++vdtIter) 
        {
          ssVert >> (*vdtIter);
        }
      }

      // Save Indices To XML tree
      std::stringstream ssIndex;;  
      ssIndex << index.getText();
      for (std::vector<std::vector<IndexDataType> >::iterator iiter=mElements->begin(); 
           iiter!=mElements->end(); ++iiter) 
      {
        for (std::vector<IndexDataType>::iterator idtIter=iiter->begin(); 
             idtIter!=iiter->end(); ++idtIter) 
        {
          ssIndex >> (*idtIter);
        }
      }
    }
    break;
    default:
    {
      // Save Vertices
      if (!getText(vertex, mVertices, storageType)) {
        vwarning("Failed to retrieve vertex data for VisusIndexedData");
        return false;
      }

      // Save Elements
      if (!getText(index, mElements, storageType)) {
        vwarning("Failed to retrieve element data for VisusIndexedData");
        return false;
      }
    }
    break;
  }

  // Save BBox To Tree
  if (! mBBox.fromXML(node)) {
    vwarning("Failed to retrieve bounding box for VisusIndexedData");
    return false;
  }

  return true;
}  

void VisusIndexedData::computeVertexNormals()
{
  uint32_t i,j;
  std::vector<IndexDataType>& poly = element(0);
  std::vector<VertexDataType> n;
  VertexDataType w;

  // First we reset all normals to 0
  for (i=0;i<nrOfVertices();i++) {
    
    (*mVertices)[i][3] = 0;
    (*mVertices)[i][4] = 0;
    (*mVertices)[i][5] = 0;
  }

  // For each element we add the normal components weighted by
  // interior angle
  for (i=0;i<nrOfElements();i++) {
    
    poly = element(i);
    
    n = weightedNormal(vertex(poly.back()),vertex(poly[0]),vertex(poly[1]));
    (*mVertices)[poly[0]][3] += n[0];
    (*mVertices)[poly[0]][4] += n[1];
    (*mVertices)[poly[0]][5] += n[2];
    
    for (j=1;j<poly.size()-1;j++) {
      n = weightedNormal(vertex(poly[j-1]),vertex(poly[j]),vertex(poly[j+1]));
      (*mVertices)[poly[j]][3] += n[0];
      (*mVertices)[poly[j]][4] += n[1];
      (*mVertices)[poly[j]][5] += n[2];
    }
    
    n = weightedNormal(vertex(poly[poly.size()-2]),vertex(poly.back()),vertex(poly[0]));
    (*mVertices)[poly.back()][3] += n[0];
    (*mVertices)[poly.back()][4] += n[1];
    (*mVertices)[poly.back()][5] += n[2];

  }

  for (i=0;i<nrOfVertices();i++) {

    w = vertex(i)[3]*vertex(i)[3] + vertex(i)[4]*vertex(i)[4] + vertex(i)[5]*vertex(i)[5];
    
    if (w > 10e-8) {
      w = 1/sqrt(w);
      //fprintf(stderr,"weight %f\n",w);
      (*mVertices)[i][3] *= -w;
      (*mVertices)[i][4] *= -w;
      (*mVertices)[i][5] *= -w;
    }
    else {
      (*mVertices)[i][3] = 0;
      (*mVertices)[i][4] = 0;
      (*mVertices)[i][5] = -1;
    }      

    //fprintf(stderr,"Normal %f %f %f \n",(*mVertices)[i][3],(*mVertices)[i][4],(*mVertices)[i][5]);
  }
}

std::vector<VisusIndexedData::VertexDataType> VisusIndexedData::edgeNormal(const std::vector<VertexDataType>& u,
                                                         const std::vector<VertexDataType>& v) const
{
  std::vector<VertexDataType> n(3);
  std::vector<VertexDataType> cross(3);
  VertexDataType mag;

  cross[0] = u[1]*v[2] - u[2]*v[1];
  cross[1] = u[2]*v[0] - u[0]*v[2];
  cross[2] = u[0]*v[1] - u[1]*v[0];

  n[0] = cross[1] * (u[2] - v[2]) - cross[2] * (u[1] - v[1]);
  n[1] = cross[2] * (u[0] - v[0]) - cross[0] * (u[2] - v[2]);
  n[2] = cross[0] * (u[1] - v[1]) - cross[1] * (u[0] - v[0]);

  mag = n[0]*n[0] + n[1]*n[1] + n[2]*n[2];

  if (mag > 10e-8) {
    mag = sqrt(mag);

    n[0] /= mag;
    n[1] /= mag;
    n[2] /= mag;
  }
  else {
    n[0] = 0;
    n[1] = 0;
    n[2] = 1;
  }

  return n;
}
    
std::vector<VisusIndexedData::VertexDataType> VisusIndexedData::weightedNormal(const std::vector<VertexDataType>& u,
                                                             const std::vector<VertexDataType>& v,
                                                             const std::vector<VertexDataType>& w) const
{
  std::vector<VertexDataType> n(3);
  VertexDataType mag;
  VertexDataType weight;

  n[0] = (u[1] - v[1])*(w[2]-v[2]) - (u[2] - v[2])*(w[1] - v[1]);
  n[1] = (u[2] - v[2])*(w[0]-v[0]) - (u[0] - v[0])*(w[2] - v[2]);
  n[2] = (u[0] - v[0])*(w[1]-v[1]) - (u[1] - v[1])*(w[0] - v[0]);
  
  mag = n[0]*n[0] + n[1]*n[1] + n[2]*n[2];
  weight = interiorAngle(u,v,w);
  //weight = 1;

  if (mag > 10e-8) {
    mag = weight/sqrt(mag);

    n[0] *= mag;
    n[1] *= mag;
    n[2] *= mag;
  }
  else {
    n[0] = 0;
    n[1] = 0;
    n[2] = weight;
  }
 
  //fprintf(stderr,"Normal %f %f %f \n",n[0],n[1],n[2]);
  return n;
}

VisusIndexedData::VertexDataType VisusIndexedData::interiorAngle(const std::vector<VertexDataType>& u,
                                               const std::vector<VertexDataType>& v,
                                               const std::vector<VertexDataType>& w) const
{
  VertexDataType angle;
  VertexDataType mag;

  angle = ((u[0]-v[0]) * (w[0]-v[0]) + (u[1]-v[1]) * (w[1]-v[1]) + (u[2]-v[2]) * (w[2]-v[2]));
  mag =  sqrt((u[0]-v[0])*(u[0]-v[0]) + (u[1]-v[1])*(u[1]-v[1]) + (u[2]-v[2])*(u[2]-v[2]));
  if (mag < 10e-4)
    return 0;

  angle /= mag;

  mag = sqrt((w[0]-v[0])*(w[0]-v[0]) + (w[1]-v[1])*(w[1]-v[1]) + (w[2]-v[2])*(w[2]-v[2]));
  if (mag < 10e-4)
    return 0;
  
  angle /= mag;

  angle = acos(angle);
  
  return angle;
}
