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


#include "VisusSphereProjection.h"
#include "math.h"


VisusSphereProjection::VisusSphereProjection() : mRadius(1)
{
}
VisusSphereProjection::~VisusSphereProjection()
{
}

VisusProjection* VisusSphereProjection::clone() const 
{
  return new VisusSphereProjection(*this);
}

VisusSphereProjection& VisusSphereProjection::operator=(const VisusSphereProjection& projection)
{
  mRadius = projection.mRadius;
  return *this;
}

int VisusSphereProjection::declareParameters(pVisusGroup caller)
{
  return caller->declareParameter(VisusSharedEarthRadius::sTypeIndex);
}

int VisusSphereProjection::updateShared(pVisusGroup info)
{
  VisusEarthRadius radius;

  if (info->getValue(radius) == 0)
    return 0;

  if (radius != mRadius) {
    mRadius = radius;
    return 2;
  }

  return 1;
}

bool VisusSphereProjection::isCompatibleInput(const VisusIndexedData* data) const
{
  VisusUnit unit = data->unit();

  return ((unit == VISUS_POLAR_DEGREES) || (unit == VISUS_POLAR_RADIANTS));
}


int VisusSphereProjection::projectVertex(const std::vector<VertexDataType>& source, std::vector<VertexDataType>& sink,
                                         VisusUnit unit)
{
 	VertexDataType longitude = source[0];
	VertexDataType latitude  = source[1];
	VertexDataType elevation = source[2];
	VertexDataType temp;

  if (unit == VISUS_POLAR_DEGREES) {
    longitude *= M_PI / 180;
    latitude *= M_PI / 180;

  }
  
	sink[0] = (mRadius + elevation) * cos(longitude) * cos(latitude);
	sink[1] = (mRadius + elevation) * sin(longitude) * cos(latitude);
  sink[2] = (mRadius + elevation) * sin(latitude);
  
  /*
  sink[3] = source[3]*cos(longitude) - source[5]*sin(longitude);
  sink[4] = source[4];
  sink[5] = source[3]*sin(longitude) + source[5]*cos(longitude);
  
  sink[3] = sink[3]*cos(latitude) - sink[4]*sin(latitude);
  sink[4] = sink[3]*sin(latitude) + sink[4]*cos(latitude);
  
  temp = 1/sqrt(sink[3]*sink[3]+sink[4]*sink[4]+sink[5]*sink[5]);
  sink[3] = sink[3]*temp;
  sink[4] = sink[4]*temp;
  sink[5] = sink[5]*temp;
  */
  // printf("normal = %f, %f, %f, %f\n", sink[3], sink[4], sink[5],temp);
	return 1;
}

