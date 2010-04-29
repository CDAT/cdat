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


#ifndef VISUSEARTHNODE_H
#define VISUSEARTHNODE_H

#include "VisusConsumer.h"
#include "VisusGroup.h"
#include "VisusSmartPointer.h"
#include "VisusSphere.h"
#include "VisusTexture.h"
#include "VisusVersioned.h"
#include "VisusIndexedData.h"

class VisusEarthNode;
typedef VisusSmartPointer<VisusEarthNode> pVisusEarthNode;

//! Node to draw data in polar coordinates on the sphere.
/*! The EarthNode draws two-dimensional data in polar coordinates onto
 *  the globe. The data can be represented as either a height field
 *  using a triangulated surface, a colormap, or using two input
 *  fields a height field colored using another piece of data. The
 *  node uses the [-180,-90]x[180,90] polar bounding box to draw and
 *  all incoming data should takes this into account. Furthermore, the
 *  node draws in "earth centered" coordinates which specify
 *
 * - The origin is at the center of the sphere
 * - The Z axis is aligned with the polar axis and increases toward the geographic North Pole.
 * - The X axis is defined by the intersction of the equatorial plane and the Prime Meridian.
 * - The Y axis is selected to complete a right-handed coordinate system.
 *
 *  This results in the conversion
 * 
 * X = H * cos(lat) * cos(long)
 * Y = H * cos(lat) * sin(long)
 * Z = H * sin(lat)
 * 
 * where long \in [-180,180] and lat \in [-90,90] 
 *
 * If presented with nodes in different periodic quadrants, e.g. [540,
 * 900] the node will attempt to roll these into the correct
 * range. However, the node is not capable of displaying data in
 * [0,360], for example, as this would require a re-arrangement of the
 * data.Setup the mGeometryScaling
 */
class VisusEarthNode : public VisusGroup, public VisusConsumer
{
public:
  static const int sDefaultSlices;
  static const int sDefaultStacks;
  static const float sDefaultGeometryScaling;

  static pVisusEarthNode instantiate();

  VisusEarthNode();
  virtual ~VisusEarthNode() {}


  //! Set the number of slices and stacks used to draw 
  int resolution(int slices, int stacks);
  
  //! Return whether the incoming data is periodic
  bool periodic() {return mPeriodic;}

  //! Indicate whether the incoming data is periodic
  void periodic(bool p);


  //! Connect to the height field producer
  int connectHeight(pVisusProducer producer);
 
  //! Connect to the texture producer
  int connectTexture(pVisusProducer producer);
 
  //! Directly load a height field
  virtual int loadHeight(VisusData* data);

  //! Directly load a texture
  virtual int loadTexture(VisusData* data);

  //! Overloaded synchronize call to force an update of the geometry data
  virtual int synchronize(int n=-1);
  
  //! Set the GeometryScaling
  void setGeometryScaling(float scale){ mGeometryScaling = scale;}

protected:
  void toXMLLocalVariables(XMLNode& node);
  bool fromXMLLocalVariables(XMLNode& node);

  //! Draw The Earth
  virtual void display3D(VisusTransformation3D model_view_3D);

private:

  //! The maximium number of slices used to render the sphere
  int mSlices;

  //! The maximum number of stacks used to render the sphere
  int mStacks;


  //! Flag indicating whether the data is periodic in longitude
  bool mPeriodic;

  //! The geometry scaling in percent of the earth radius
  float mGeometryScaling;

  //! Mutex to protect the pre-computed data and parameters
  VisusReadWriteLock mAccessLock;

  //! The incoming texture data
  VisusTexture mTextureData;

  //! The incoming geometry data
  VisusBlockData mHeightData;
  
  //! The mesh of the heigh field
  VisusIndexedData mGeometryData;

  //! The last data id of the geometry data
  VisusDataID mHeightId;

  //! The last version of the color map used
  VisusVersionNumber mColorMapVersion;
  
  //! Compute the unit coordinates of a given latitude longitude pair
  void unitCoordinates(float longitude, float latitude, float coord[3]);
    
  //! Reconstruct the mesh for the height field
  void updateGeometry();

  //! Render the data
  void renderGeometry();
  
  //! Compute the the texture transformation
  void textureTransform(float u_bounds[2], float v_bounds[2]);

  //! Render a sphere of radius 1 with standard texture coordinates
  void renderSphere(float u_bounds[2], float v_bounds[2]);

  void computeMeshNormal(int x, int y, int nx, int ny, int periodic_type, float n[3]);
  
  void computeMeshNormal(int center, int right, int left, float n[3]);

  template <typename DataType>
  void constructGeometry(const std::vector<double>& left,const std::vector<double>& right); 

};

template <typename DataType>
void VisusEarthNode::constructGeometry(const std::vector<double>& left,const std::vector<double>& right)
{
  float scale;
  float dx,dy;
  double range[2];
  float v[3];
  float longitude,latitude;
  const DataType* buffer;
  int i,j;
  int count = 0;
  int periodic_type;
  
  dx = (right[0] - left[0]) / (mHeightData.samples()[0]-1);    
  dy = (right[1] - left[1]) / (mHeightData.samples()[1]-1);
  
  
  buffer = (const DataType*)(mHeightData.data());
  
  longitude = left[0];
  while (longitude < -M_PI-0.1)
    longitude += 2*M_PI;
  while (longitude > M_PI+0.1)
    longitude -= 2*M_PI;
  
  if ((right[0] - left[0]) > 2*M_PI-0.001) { 
    // If the data spans 2*pi in longitude the data is in itself
    // periodic meaning the first and last row are identical which we
    // must take into account when computing the normals.
    periodic_type = 0;
  }
  else if ((right[0] - left[0]) > 2*M_PI-0.001-2*M_PI/(mHeightData.samples()[0]-1)) {
    // If the data looks like it could be draw periodic but the last
    // column is not repeated.
    periodic_type = 1;
  }
  else {// The data is not periodic
    periodic_type = 2;
  }
    

 
  latitude = left[1];
  while (latitude < -M_PI_2-0.1)
    latitude += M_PI;
  while (latitude > +M_PI_2+0.1)
    latitude -= M_PI;
  

  mHeightData.getFieldRange(range[0],range[1]);

  if (range[1] - range[0] > 0.000001)
    scale = mGeometryScaling / (range[1] - range[0]);
  else
    scale = 0;

  // We will have to write the geometry data and thus need the lock
  if (mAccessLock.writeLock() == 0) {
    vwarning("Could not obtain read-lock for rendering.");
    return;
  }

  // We always compute a grid with one extra column to handle possible
  // periodic boundaries
  mGeometryData.reserveVertices((mHeightData.samples()[0]+1)*mHeightData.samples()[1]);
  mGeometryData.reserveElements(1);
  
  // We save the number of samples on each axis in the first element
  mGeometryData.element(0)[0] = mHeightData.samples()[0];
  mGeometryData.element(0)[1] = mHeightData.samples()[1];

  int ni = mHeightData.samples()[0]+1;
  int nj = mHeightData.samples()[1];
  // First we compute the geometric coordinates as well was the
  // texture coordinates
  for (int j=0;j<mHeightData.samples()[1];j++) {
    
    longitude = left[0];

    for (int i=0;i<mHeightData.samples()[0];i++) {

      unitCoordinates(longitude,latitude,v);

      mGeometryData.vertex(count)[0] = (1 + scale*(buffer[j*(ni-1)+i]-range[0]))*v[0];
      mGeometryData.vertex(count)[1] = (1 + scale*(buffer[j*(ni-1)+i]-range[0]))*v[1];
      mGeometryData.vertex(count)[2] = (1 + scale*(buffer[j*(ni-1)+i]-range[0]))*v[2];

      if (longitude > 2*M_PI)
        mGeometryData.vertex(count)[6] = longitude-2*M_PI;
      else
        mGeometryData.vertex(count)[6] = longitude;
        
      if (latitude > M_PI)
        mGeometryData.vertex(count)[7] = latitude - M_PI;
      else
        mGeometryData.vertex(count)[7] = latitude;
        
      
      longitude += dx;
      count++;
    }

    // Make sure that the periodic column has *exactly* the same
    // coordinates as the first
    mGeometryData.vertex(count)[0] = mGeometryData.vertex(count-mHeightData.samples()[0])[0];
    mGeometryData.vertex(count)[1] = mGeometryData.vertex(count-mHeightData.samples()[0])[1];
    mGeometryData.vertex(count)[2] = mGeometryData.vertex(count-mHeightData.samples()[0])[2];
    if (longitude > 2*M_PI)
      mGeometryData.vertex(count)[6] = longitude-2*M_PI;
    else
      mGeometryData.vertex(count)[6] = longitude;
    
    if (latitude > M_PI)
      mGeometryData.vertex(count)[7] = latitude - M_PI;
    else
      mGeometryData.vertex(count)[7] = latitude;

    longitude += dx;
    count++;
    
    latitude += dy;
  }

  
  float n[3];
  for (j=0;j<mHeightData.samples()[1];j++) {

    for (i=0;i<mHeightData.samples()[0];i++) {

      computeMeshNormal(i,j,ni,nj,periodic_type,n);

      mGeometryData.vertex(j*ni + i)[3] = n[0];
      mGeometryData.vertex(j*ni + i)[4] = n[1];
      mGeometryData.vertex(j*ni + i)[5] = n[2];
    }
  }

  if (periodic_type == 0) {
    for (j=0;j<mHeightData.samples()[1];j++) {
      
      mGeometryData.vertex(j*ni + ni - 1)[3] = mGeometryData.vertex(j*ni + 1)[3];
      mGeometryData.vertex(j*ni + ni - 1)[4] = mGeometryData.vertex(j*ni + 1)[4];
      mGeometryData.vertex(j*ni + ni - 1)[5] = mGeometryData.vertex(j*ni + 1)[5];
    }
  }
  else {
    for (j=0;j<mHeightData.samples()[1];j++) {
      
      mGeometryData.vertex(j*ni + ni - 1)[3] = mGeometryData.vertex(j*ni)[3];
      mGeometryData.vertex(j*ni + ni - 1)[4] = mGeometryData.vertex(j*ni)[4];
      mGeometryData.vertex(j*ni + ni - 1)[5] = mGeometryData.vertex(j*ni)[5];
    }
  }


  if (mAccessLock.unlock() == 0) 
    vwarning("Could not unlock local access mutex.");
}




#endif
