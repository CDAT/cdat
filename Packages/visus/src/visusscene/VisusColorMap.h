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


#ifndef VISUSCOLORMAP_H
#define VISUSCOLORMAP_H

#if WIN32
#include <windows.h>
#endif

#include <math.h>

#include "glew.h"
#include "VisusAssert.h"
#include "VisusDefinitions.h"
#include "VisusVersioned.h"

struct XMLNode;

enum VisusColorMapType {
  VISUS_GREY_SCALE  = 0,
  VISUS_GREY_RAMP   = 1,
  VISUS_BANDED      = 2,
  VISUS_BGRY        = 3,
  VISUS_BRY         = 4,
  VISUS_GAMMA       = 5,
  VISUS_HOT1        = 6,
  VISUS_HOT2        = 7,
  VISUS_ICE         = 8,
  VISUS_LIGHTHUES   = 9,
  VISUS_LUT16       = 10,
  VISUS_RICH        = 11,
  VISUS_SMOOTH_RICH = 12,
  VISUS_CUSTOM_MAP  = 13,
};

enum VisusColorMapStyle {
  VISUS_LINEAR_MAP = 0,
  VISUS_LOG_MAP    = 1,
  VISUS_EXP_MAP    = 2,
};

//! Interface for all color maps of the Visus system
/*! This class implements the basic functionality of and serves as an
 *  interface for general color maps in the Visus system. The map is
 *  based on a floating point values list of colors that at run-time
 *  will be translated into OpenGL colors. The standard interface is
 *  build on mapping the [0,1] interval to the given list of
 *  colors. However, there are functions that adjust this range to be
 *  bigger and or smaller. Furthermore, a color map stores a min and
 *  max colors that will be displayed for values below/above the
 *  current range.
 */
class VisusColorMap : public VisusVersioned
{
public:

  typedef int (VisusColorMap::*MapFunctionPointer)(float) const;

  static const char* XML_TAG;

  VisusColorMap(int resolution=256, VisusColorMapType t=VISUS_CUSTOM_MAP);

  VisusColorMap(const VisusColorMap& map);

  ~VisusColorMap();

  VisusColorMap& operator=(const VisusColorMap& map);
  
  //! Direct access to individual colors but not the floor or ceiling
  //! color
  float &operator[](int i) {vassert(i<4*mResolution);return mMap[4+i];}

  //! Return the current resolution
  int resolution() const {return mResolution;}

  //! For the function value f get the corresponding color
  int getColor(float f,unsigned char* color) const;
  
  //! For the function value f get the corresponding color
  int getColor(float f,float *color) const;

  //! Return the floor color
  int getFloorColor(unsigned char* color) const;

  //! Return the floor color
  int getFloorColor(float* color) const;

  //! Return the ceiling color
  int getCeilingColor(unsigned char* color) const;

  //! Return the ceiling color
  int getCeilingColor(float* color) const;

  //! Return the OpenGL id of this texture
  GLuint texId();

  //! Indicate whether this color map has been initialized
  bool initialized() {return mInitialized;}

  //! Return the color map type
  VisusColorMapType type() const {return mType;}

  //! Return the color map style
  VisusColorMapStyle style() const {return mStyle;}

  //! Get the current bounds
  int getBounds(double& low, double &high) const;

  //! Set the map style and corresponding bounds
  /*! This call sets the mapping style and fucntion range. A low=high
   *  0-sized function range indicates that the map should be
   *  considered uninitialized and use the min-max values of whatever
   *  data it is being applied to
   *  @param style: new mapping style
   *  @param low: lower bound on the mapped function range (default 0)
   *  @param high: upper bound on the mapped function range (default 0)
   */
  int style(VisusColorMapStyle style, double low=0, double high=0);

  //! Set the floor color
  int setFloorColor(float r, float g, float b, float a=1);

  //! Set the ceiling color
  int setCeilingColor(float r, float g, float b, float a=1);

  //! Convinience function to set linear map style
  int linearMap(double low=0, double high=0) {return style(VISUS_LINEAR_MAP,low,high);}

  //! Convinience function to set logarithmic map style
  int logarithmicMap(double low=0, double high=0) {return style(VISUS_LOG_MAP,low,high);}

  //! Convinience function to set exponential map style
  int exponentialMap(double low=0, double high=0) {return style(VISUS_EXP_MAP,low,high);}

  //! Set new function range bounds
  /*! This calls adjust the function range bounds. Similar to the
   *  functions that also adjust the style setBounds will interpret a
   *  low=high as a request for reinitialization.
   *  @param low: lower bound on the mapped function range (default 0)
   *  @param high: upper bound on the mapped function range (default 0)   
   */
  int setBounds(double low, double high) {return style(mStyle,low,high);}

  //! Build xml instance data from XML tree
  bool fromXML(XMLNode& node);

  //! Build xml instance data into XML tree
  void toXML(XMLNode& parent) const;

private:

  //! The type of color map currently used
  VisusColorMapType mType;

  //! The current mapping style
  VisusColorMapStyle mStyle;

  //! Flag indicating whether this map has been initialized
  bool mInitialized;

  //! The current mapping function used
  MapFunctionPointer mMapFunction;

  //! The last lower bound that was used
  double mLow;

  //! The last upper bound that was used
  double mHigh;

  //! The adjusted low bound of the function range
  /*! The function value in the linear case would typically be mapped
   *  by something like (f-low)/(high-low) * resolution. To reduce
   *  computations we pre-store (high-low) in form of the map
   *  span. Furthermore, we want to round the result to integers by
   *  truncation and thus compute (f-low)/(high-low) * resolution +
   *  0.5. We fold this computation into the minimal number of
   *  operations be expressing it as (f - mMapAnchor) * mMapScale,
   *  where mMapAnchor is low - 0.5*mMapSpan/mResolution. Note that
   *  the map anchor may work differently for different projections
   */
  double mMapAnchor;
  
  //! The current span of the map see mMapAnchor
  double mMapScale;

  //! Pointer to store the colormap
  /*! Pointer to a float[mResolution+2][4] style array storing the
   *  colors as red,green,blue,alpha values. The first and last colors
   *  are used for values outside the specified range
   */
  float* mMap;

  //! Color map resolution
  int mResolution;

  //! The OpenGL id of the texture if 
  GLuint mTexId;

  //! Linearly map f to the color space
  int linearMapping(float f) const;

  //! Logarithmically map f to the color space
  int logarithmicMapping(float f) const;

  //! Exponentially map f to the color space
  int exponentialMapping(float f) const;
  
};

inline int VisusColorMap::linearMapping(float f) const
{
  return static_cast<int>(MIN(MAX((f-mMapAnchor)*mMapScale,0),mResolution+1)) << 2;
}

inline int VisusColorMap::logarithmicMapping(float f) const
{
  return static_cast<int>(MIN(MAX(log(MAX((f - mMapAnchor)*mMapScale,0.000001))*(mResolution-1)+ 1.5,0),mResolution+1)) << 2;
}


inline int VisusColorMap::exponentialMapping(float f) const
{
  return static_cast<int>(MIN(MAX((exp((f - mMapAnchor)*mMapScale) - 1)*(mResolution-1)+ 1.5,0),mResolution+1)) << 2; 
}

#endif
