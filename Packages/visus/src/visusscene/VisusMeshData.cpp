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


#include <assert.h>
#include <math.h>

#include "xmlParser.h"

#include "VisusMeshData.h"
#include "VisusDefinitions.h"
#include "VisusAssert.h"
#include "VisusXMLInterface.h"


VisusMeshData::VisusMeshData(uint8_t vertex_dim, uint8_t element_dim) 
  : VisusIndexedData(vertex_dim,VISUS_MESH_DATA), mElementDim(element_dim)
{
  if (mElementDim == 0) {
    vwarning("VisusMeshData constructed with 0-dimenional elements");
  }

  mNormals = new std::vector<std::vector<VertexDataType> >;
}
                                                                        

VisusMeshData::VisusMeshData(const VisusMeshData& mesh) 
  : VisusIndexedData(mesh), mElementDim(mesh.mElementDim)
{
  *mNormals = *mesh.mNormals;
}


VisusMeshData::~VisusMeshData()
{
  delete mNormals;
}



const VisusMeshData::VertexDataType* VisusMeshData::elementNormalAddress(IndexDataType i) const
{
  return &((*mNormals)[i][0]);
}


int VisusMeshData::reserveElements(int n)
{
  try {
    mElements->resize(n,std::vector<IndexDataType>(mElementDim,0));
  }
  catch (const std::bad_alloc& /*error*/) {
    vwarning("Could not allocate enough memory for elements.");
    return 0;
  }

  return 1;
}


int VisusMeshData::swapContent(VisusData* data) 
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

  // Swap all the index data
  VisusIndexedData::swapContent(data);

  // No need to perform fancy dynamic casting since the swappable
  // function should have done all the testing we need
  VisusMeshData* mesh = (VisusMeshData*)data;

  std::swap(mElementDim,mesh->mElementDim);
  std::swap(mNormals,mesh->mNormals);

  return 1;
}


int VisusMeshData::copyContent(const VisusData* data)
{
  // If I can't read the input I cannot proceed
  if (!readCompatible(data)) {
    vwarning("Data types not compatible cannot copy.");
    return 0;
  }

  VisusIndexedData::copyContent(data);

  VisusMeshData* mesh = (VisusMeshData*)data;

  mElementDim = mesh->elementDim();
  *mNormals = *(mesh->mNormals);

  return 1;
}
  
bool VisusMeshData::readCompatible(const VisusData* data) const
{
  return (data->dataFormat() == VISUS_MESH_DATA);
}


int VisusMeshData::loadObj(const char* filename)
{
  int nr_vert = 0;
  int nr_norm = 0;
  int nr_face = 0;

  FILE *input;
  char line[100];
  float bbox[6];

  input = fopen(filename,"r");
  if (input == NULL) {
    vwarning("Could not open file.");
    return 0;
  }

  while (fgets(line, sizeof(line), input)) {
    
    if (strncmp(line,"v ",2) == 0) 
      nr_vert++;
    else if (strncmp(line,"vn ",2) == 0) 
      nr_norm++;
    else if (strncmp(line,"f ",2) == 0) 
      nr_face++;
  }

  fclose(input);

  nr_norm = 0;
  if ((nr_norm != 0) && (nr_norm != nr_vert)) {
    vwarning("Input mesh inconsistent. Number of normals must match number of vertices.");
    return 0;
  }

  if (nr_norm > 0) 
    mVertexDim = 6;
  else
    mVertexDim = 3;

  // For now we can only handle triangle meshes
  mElementDim = 3;

  reserveVertices(nr_vert);
  reserveElements(nr_face);


  input = fopen(filename,"r");
  
  nr_vert = 0;
  nr_norm = 0;
  nr_face = 0;
  
  bbox[0] = bbox[1] = bbox[2] = 10000000;
  bbox[3] = bbox[4] = bbox[5] = -10000000;

  while (fgets(line, sizeof(line), input)) {
    
    if (strncmp(line,"v ",2) == 0) {
      sscanf(line+2,"%f %f %f",&(vertex(nr_vert)[0]),&(vertex(nr_vert)[1]),&(vertex(nr_vert)[2]));

      bbox[0] = MIN(bbox[0],vertex(nr_vert)[0]);
      bbox[1] = MIN(bbox[1],vertex(nr_vert)[1]);
      bbox[2] = MIN(bbox[2],vertex(nr_vert)[2]);

      bbox[3] = MAX(bbox[3],vertex(nr_vert)[0]);
      bbox[4] = MAX(bbox[4],vertex(nr_vert)[1]);
      bbox[5] = MAX(bbox[5],vertex(nr_vert)[2]);
    
      nr_vert++;
    }
    else if ((mVertexDim >= 6) && (strncmp(line,"vn ",2) == 0)) {
      sscanf(line+3,"%f %f %f",&(vertex(nr_norm)[3]),&(vertex(nr_norm)[4]),&(vertex(nr_norm)[5]));
      nr_norm++;
    }
    else if (strncmp(line,"f ",2) == 0) {
      sscanf(line+2,"%d %d %d",&(element(nr_face)[0]),&(element(nr_face)[1]),&(element(nr_face)[2]));
      element(nr_face)[0]--;
      element(nr_face)[1]--;
      element(nr_face)[2]--;
      nr_face++;
    }
  }

  fclose(input);

  mBBox.set(bbox);

  if (nr_norm == 0) {
    mNormals = new std::vector<std::vector<VertexDataType> >;
    mNormals->resize(nr_face,std::vector<VertexDataType>(3,0));
    VertexDataType u[3];
    VertexDataType v[3];
    VertexDataType n[3];
    float mag;

    for (int i=0;i<nr_face;i++) {
      for (int j=0;j<3;j++) {
        u[j] = vertex(element(i)[1])[j] - vertex(element(i)[0])[j];
        v[j] = vertex(element(i)[2])[j] - vertex(element(i)[0])[j];
      }

      n[0] = u[1]*v[2] - u[2]*v[1];
      n[1] = u[2]*v[0] - u[0]*v[2];
      n[2] = u[0]*v[1] - u[1]*v[0];
      mag = sqrt(n[0]*n[0] + n[1]*n[1] + n[2]*n[2]);

      if (mag < 0.00001) 
        n[0] = n[1] = n[2] = 0;
      else {
        (*mNormals)[i][0] = n[0];
        (*mNormals)[i][1] = n[1];
        (*mNormals)[i][2] = n[2];
      }
    }
  }


  fprintf(stderr,"Loaded Obj mesh %d vertices %d elements\n",nrOfVertices(),nrOfElements());
  return 1;
}
  
void VisusMeshData::toXMLLocalVariables(XMLNode& parent) const
{
  VisusIndexedData::toXMLLocalVariables(parent);
  
  parent.addAttribute("elementDim", mElementDim);

  // Save Vertices To XML tree
  XMLNode node = parent.addChild("normals");
  switch (VisusXMLInterface::sWriteXMLDataStorage)
  {
    case ASCII:
    {
      // Save data as ASCII
      std::stringstream ss;
      for (std::vector<std::vector<VertexDataType> >::const_iterator viter=mNormals->begin(); 
           viter!=mNormals->end(); ++viter) 
      {
        for (std::vector<VertexDataType>::const_iterator vdtIter=viter->begin(); 
             vdtIter!=viter->end(); ++vdtIter) 
        {
          ss << (*vdtIter) << " ";
        }
      }
      node.addText(ss.str().c_str());
    }
    break;
    default:
      addText(node, mNormals);
  }
}
  
bool VisusMeshData::fromXMLLocalVariables(XMLNode& node, XMLDataStorage storageType)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusMeshData did not receive top level node");
    return false;
  }

  if (! VisusIndexedData::fromXMLLocalVariables(node, storageType))
    return false;
  
  mElementDim = xmltoi(node.getAttribute("elementDim"), mElementDim);

  // Save Vertices To XML tree
  XMLNode child = node.getChildNode("normals");
  switch (storageType)
  {
    case ASCII:
    {
      // Save data as ASCII
      std::stringstream ss;
      ss << child.getText();
      for (std::vector<std::vector<VertexDataType> >::iterator viter=mNormals->begin(); 
           viter!=mNormals->end(); ++viter) 
      {
        for (std::vector<VertexDataType>::iterator vdtIter=viter->begin(); 
             vdtIter!=viter->end(); ++vdtIter) 
        {
          ss >> (*vdtIter);
        }
      }
    }
    break;
    default:
      if (! getText(node, mNormals, storageType)) {
        vwarning("Failed to retrieve normals for VisusMeshData");
        return false;
      }
  }
  return true;
}
  




