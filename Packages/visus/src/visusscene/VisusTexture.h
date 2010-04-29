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


#ifndef VISUSTEXTURE_H
#define VISUSTEXTURE_H

#ifdef WIN32
#include <windows.h>
#endif

#include "glew.h"

#include "VisusBlockData.h"
#include "VisusColorMap.h"

/*! The texture class in many respects is a specialized BlockData
 *  type. However, for reason explained below it is not a sub-class of
 *  VisusBlockData. A VisusTexture provides an interface to a 2D array
 *  of data in form of a texture. After a calling preDraw() any
 *  geometry drawn with approriate texture coordinates will display
 *  the data. Note, that the geometry can be as coarse as a single
 *  quad. Since OpenGL textures exist for only a sub-set of data
 *  types, for example there exists no float64/double texture, a
 *  VisusTexture is not derived from VisusBlockData even though in
 *  many cases it functionality is similar.
 *
 *  A VisusTexture can operate in two different modes depending on the
 *  available hardware and/or data type that is present. If no
 *  programmable shaders exists or the data is raw imagery (e.g
 *  PV_RGB) the data is converted/copied into a 2D texture with the
 *  same resolution as the the incomping data. This texture is then
 *  drawn in the standard manner. In particular, any necessary
 *  up-scaling is handled in RGB space by the OpenGL pipeline. If
 *  programmable shader hardware is present The data is first
 *  converted to an OpenGL native data type (e.g double must become
 *  floats, int64's int32's etc.) and passed as texture to the
 *  shader. The shader will use the resulting (interpolated) values
 *  for a per-fragment lookup into a secondary 1D texture to produce
 *  the final color. The primary advantage of the second approach is
 *  that any necessary upscaling happens in function space. This
 *  allows a much higher resolution transfer function as well as a
 *  more efficient rendering as the data needs not to be reloaded as
 *  the color-map changes. 
 *
 *  It is important to note that missing values represent an
 *  additional challenge to both schemes. The problem is not so much
 *  the masking of values itself but the interpolation necessary when
 *  up-scaling. This interpolation naturally uses the \e bad data
 *  values and thus might introduce artifacts. The solution to these
 *  problems is described in details in the rendering code.
 */
class VisusTexture : public VisusBlockData
{
public:

  static const bool sUseHardwareShaders;

  VisusTexture();

  virtual ~VisusTexture() {}

  GLuint textureTarget() const {return sTextureTarget;}

  //! Return the currnt maximal texture coordinates
  std::vector<float> texMax() {return mTexMax;}

  void texMax(float *u, float *v) {*u=mTexMax[0],*v=mTexMax[1];}

  int preDraw(VisusColorMap& color_map);

  int postDraw();

  int convertToRGBA(const VisusColorMap& color_map);

  int convertToInternal();
  
  virtual int swapContent(VisusData* data);

  virtual int copyContent(const VisusData* data);

  //! Determine whether \e this can load it contents from data
  virtual bool readCompatible(const VisusData* data) const;

  //! For the current set of dimensions, samples number, data type,
  //  etc. allocate sufficient memory
  //virtual int reserveSpace();

  //! Load a PPM file into the VisusTexture
  int loadPPM(const char *file);

  //! Extract the the last and first sample in the given dimension
  int extractPeriodicBoundary(int axis, VisusTexture& boundary) const;

  //! For the current set of dimensions, samples number, data type,
  //  etc. allocate sufficient memory
  virtual int reserveSpace();

protected:
  void toXMLLocalVariables(XMLNode& parent) const;
  void saveXMLData(XMLNode& node) const;

  bool fromXMLLocalVariables(XMLNode& parent, XMLDataStorage storageType);
  bool loadXMLData(XMLNode& node, XMLDataStorage storageType);

private:
  static const int sShaderLength;
  static const char* sShaderCode[];
  static const GLuint sTextureTarget;

  //! Loaded file
  std::string mFilename;

  //! Maximal texture coordinate
  std::vector<float> mTexMax;
 
  //! The id of the last data set processed
  VisusDataID mLastId;

  //! Texture id 
  GLuint mTexId;

  //! Texture id of the mask 
  GLuint mMaskId;

  int compileShader();
};

#endif
