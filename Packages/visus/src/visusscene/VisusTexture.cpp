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


#include <cstdlib>
#include <iostream>
#include <fstream>

#include "glew.h"
#include "xmlParser.h"

#include "VisusAssert.h"
#include "VisusStdInt.h"
#include "VisusTexture.h"

static GLhandleARB sIndirectColorMap = 0;

const bool VisusTexture::sUseHardwareShaders = false;
//const bool VisusTexture::sUseHardwareShaders = GL_ARB_vertex_program && GL_TEXTURE_RECTANGLE_ARB;

const int VisusTexture::sShaderLength = 17;
const char* VisusTexture::sShaderCode[] = {
  "uniform sampler1D color_map;",
  "uniform sampler2DRect function_map;",
  "uniform sampler2DRect mask;",
  "uniform float min_func;",
  "uniform float span;",
  "void main()",
  "{",
  "  float f;",
  "  float m = texture2DRect(mask,vec2(gl_TexCoord[0])).r;",
  "  if(m > 0.001) {",
  "    f = texture2DRect(function_map,vec2(gl_TexCoord[0])).r;",
  "    gl_FragColor = texture1DProj(color_map,vec2(f-min_func,span));",
  "  }",
  "  else {",
  "    gl_FragColor = vec4(1.0,0.0,0.0,1.0);",  
  "  }",
  "}"
};
const GLuint VisusTexture::sTextureTarget = GL_TEXTURE_RECTANGLE_ARB;
//const GLuint VisusTexture::sTextureTarget = GL_TEXTURE_2D;


VisusTexture::VisusTexture() : VisusBlockData(), mFilename(""), 
                               mTexMax(2,1),
                               mLastId(NULLID),mTexId(0),
                               mMaskId(0)
{ 
  //fprintf(stderr,"Use hardware %d\n",sUseHardwareShaders);
  if (sUseHardwareShaders && (sIndirectColorMap == 0))
    compileShader();

  mDataFormat = VISUS_TEXTURE_DATA;
}


int VisusTexture::preDraw(VisusColorMap& color_map)
{
  // We cannot determine what to do with raw data
  if (dataType() == PV_RAW) {
    //vwarning("Cannot create a texture from PV_RAW.");
    return 0;
  }
  
  // If hardware shaders are available 
  if (sUseHardwareShaders) {
     
    // If we have not yet bound a texture or new data has come in
    if ((mTexId == 0) || (mLastId != id())) {
      mLastId = id(); // copy the id 
      
      // if necessary convert the data to a useable format and load it
      // into texture memory
      convertToInternal(); 
    }

    glUseProgramObjectARB(sIndirectColorMap);
    
    // Pass the current texture mask etc. to the shader program
    GLint map_location      = glGetUniformLocationARB(sIndirectColorMap,"color_map");
    GLint function_location = glGetUniformLocationARB(sIndirectColorMap,"function_map");
    GLint mask_location     = glGetUniformLocationARB(sIndirectColorMap,"mask");
    GLint min_location      = glGetUniformLocationARB(sIndirectColorMap,"min_func");
    GLint span_location     = glGetUniformLocationARB(sIndirectColorMap,"span");
    
    glActiveTextureARB(GL_TEXTURE0 + color_map.texId());
    glBindTexture(GL_TEXTURE_1D,color_map.texId());
    glUniform1iARB(map_location,color_map.texId());
    
    glActiveTextureARB(GL_TEXTURE0 + mTexId);
    glBindTexture(sTextureTarget,mTexId);
    glUniform1iARB(function_location,mTexId);

    glActiveTextureARB(GL_TEXTURE0 + mMaskId);
    glBindTexture(sTextureTarget,mMaskId);
    glUniform1iARB(mask_location,mMaskId);

    glUniform1iARB(mask_location,mMaskId);
    glUniform1fARB(min_location,mMinFieldValue);
    glUniform1fARB(span_location,mMaxFieldValue - mMinFieldValue);
    
  }
  else { // If no hardware shaders are available 
    // Without the ability to use an indirect texture look-up we
    // simply transform the data into a standard RGBA texture 

    // Unless the data is already transformed or new data has come in
    if ((dataType() != PV_RGBA) || (mLastId != id())) {
      mLastId = id();
      convertToRGBA(color_map);
    }
    
    glBindTexture(sTextureTarget, mTexId);
    
    glEnable(sTextureTarget);
    
  }    

  glEnable(GL_DEPTH_TEST);
  glEnable(GL_ALPHA_TEST);

  vglerror();
    
  return 1;
}

int VisusTexture::postDraw()
{
  if (sUseHardwareShaders) {
    glUseProgramObjectARB(0);
  }
  else {
    glDisable(sTextureTarget);
  }

  glDisable(GL_ALPHA_TEST);

  vglerror();
  return 1;
}


int VisusTexture::convertToRGBA(const VisusColorMap& color_map)
{
  unsigned char *alpha;
  int32_t *tmp;
  PvDataType old_type = dataType();
  int old_size = sampleSize();

  dataType(PV_RGBA);

  if (old_size < 4) {

    reserveSpace();

    alpha = getMask() + usedSamples()-1;
    tmp = (int32_t*)data() + usedSamples()-1;
    switch (old_type) {

    case PV_CHAR:{
      char *cbuffer = (char*)data() + usedSamples()-1;

      while (alpha >= getMask()) {
        color_map.getColor(*cbuffer, (unsigned char*)tmp);
        ((unsigned char*)tmp)[3] = (*alpha && ((unsigned char*)tmp)[3]) ? 255 : 0;
        --cbuffer;
        --tmp;
        --alpha;
      }
      break;
                 }
    case PV_UCHAR:{
      unsigned char* ubuffer = data() + usedSamples()-1;
      
      while (alpha >= getMask()) {
        color_map.getColor(*ubuffer,(unsigned char*)tmp);
        ((unsigned char*)tmp)[3] = (*alpha && ((unsigned char*)tmp)[3]) ? 255 : 0;
        --ubuffer;
        --tmp;
        --alpha;
      }
      break;}
      
    case PV_RGB:
      for (int i=usedSamples()-1;i>=00;i--) {
        mData[4*i+3] = 255;
        mData[4*i+2] = mData[3*i+2];
        mData[4*i+1] = mData[3*i+1];
        mData[4*i+0] = mData[3*i+0];
      }


      break;

    case PV_INT16:
    case PV_UINT16:
      vassert(false);
      break;
      
    default:
      vassert(false);
      break;
    }
  }
  else {
    alpha = this->mMask;
    tmp = (int32_t*)data();
    unsigned char* end =  getMask() + usedSamples();
    vglerror();

    switch (old_type) {
      
    case PV_INT:
    case PV_INT32:
    case PV_UINT32:
    case PV_INT64:
    case PV_UINT64:
      vassert(false);
      break;
    case PV_FLOAT:
    case PV_FLOAT32: {
      float* fbuffer = (float*)this->mData;

      while (alpha < end) {
        color_map.getColor(*fbuffer, (unsigned char*)tmp);
        ((unsigned char*)tmp)[3] = (*alpha && ((unsigned char*)tmp)[3]) ? 255 : 0;
        //fprintf(stderr,"%d ",*alpha);
        //fprintf(stderr,"Converted sample to color <%d, %d, %d, %d>\n",((unsigned char*)tmp)[0],
        //        ((unsigned char*)tmp)[1],((unsigned char*)tmp)[2],((unsigned char*)tmp)[3]);
        ++fbuffer;
        ++tmp;
        ++alpha;
      }
      break;
                    }
    case PV_FLOAT64:{
      double* dbuffer = (double*)data();
      tmp = (int32_t*)data();
      
      while (alpha < end) {
        color_map.getColor(*dbuffer, (unsigned char*)tmp);
        ((unsigned char*)tmp)[3] = (*alpha && ((unsigned char*)tmp)[3]) ? 255 : 0;
        ++dbuffer;
        ++tmp;
        ++alpha;
      }
      break;
                    }
    default:
      vassert(false);
      break;
    }    
  }      

  if (mTexId == 0) {
    glGenTextures(1,&mTexId);

    glBindTexture(sTextureTarget, mTexId);
    
    //glTexParameterf(sTextureTarget, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    //glTexParameterf(sTextureTarget, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameterf(sTextureTarget, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(sTextureTarget, GL_TEXTURE_WRAP_T, GL_CLAMP);
    //glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    glTexParameterf(sTextureTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(sTextureTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glAlphaFunc(GL_GREATER,0);

  }
  else {
    glBindTexture(sTextureTarget, mTexId);
  }
  	
  glTexImage2D(sTextureTarget,0,GL_RGBA,this->mSamples[0],this->mSamples[1],
               0,GL_RGBA,GL_UNSIGNED_BYTE,mData);

  vglerror();
  
  return 1;
}
 
 
int VisusTexture::convertToInternal()
{
  if (mMaskId == 0) {
    glGenTextures(1,&mMaskId);

    glBindTexture(sTextureTarget, mMaskId);

    glTexParameterf(sTextureTarget, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(sTextureTarget, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
    glTexParameterf(sTextureTarget, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameterf(sTextureTarget, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  }


  glTexImage2D(sTextureTarget,0,GL_INTENSITY,this->mSamples[0],this->mSamples[1],
               0,GL_RED,GL_UNSIGNED_BYTE,mMask);

  
  if (mTexId == 0) {
    glGenTextures(1,&mTexId);

    glBindTexture(sTextureTarget, mTexId);
    
    glTexParameterf(sTextureTarget, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameterf(sTextureTarget, GL_TEXTURE_WRAP_T, GL_CLAMP);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
    glTexParameterf(sTextureTarget, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(sTextureTarget, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  }
  else {
    glBindTexture(sTextureTarget, mTexId);
  }

  switch (dataType()) {
    
  case PV_RAW:
    vwarning("Cannot create texture from PV_RAW.\n");
    break;
  case PV_CHAR:
    glTexImage2D(sTextureTarget,0,GL_INTENSITY,mSamples[0],mSamples[1],
                 0,GL_RED,GL_BYTE,mData);
    break;
  case PV_UCHAR:
    glTexImage2D(sTextureTarget,0,GL_INTENSITY,mSamples[0],mSamples[1],
                 0,GL_RED,GL_UNSIGNED_BYTE,mData);
    break;
  case PV_INT16:
    glTexImage2D(sTextureTarget,0,GL_INTENSITY,mSamples[0],mSamples[1],
                 0,GL_RED,GL_SHORT,mData);
    break;   
  case PV_UINT16:
    glTexImage2D(sTextureTarget,0,GL_INTENSITY,mSamples[0],mSamples[1],
                 0,GL_RED,GL_UNSIGNED_SHORT,mData);
    break;
  case PV_INT:
  case PV_INT32:
    glTexImage2D(sTextureTarget,0,GL_INTENSITY,mSamples[0],mSamples[1],
                 0,GL_RED,GL_INT,mData);
    break;   
  case PV_UINT32:
    glTexImage2D(sTextureTarget,0,GL_INTENSITY,mSamples[0],mSamples[1],
                 0,GL_RED,GL_UNSIGNED_INT,mData);
    break;
  case PV_INT64:{
    dataType(PV_INT32);
    
    int64_t* src64 = (int64_t*)mData;
    int32_t* target64 = (int32_t*)mData;
    
    for (int i=0;i<mSamples[0]*mSamples[i];i++)
      target64[i] = src64[i];
    
    glTexImage2D(sTextureTarget,0,GL_INTENSITY,mSamples[0],mSamples[1],
                 0,GL_RED,GL_INT,mData);
    break;
                }
  case PV_UINT64:{
    dataType(PV_UINT32);
    
    uint64_t* srcu64 = (uint64_t*)mData;
    uint32_t* targetu64 = (uint32_t*)mData;
    
    for (int i=0;i<mSamples[0]*mSamples[i];i++)
      targetu64[i] = srcu64[i];
    
    glTexImage2D(sTextureTarget,0,GL_INTENSITY,mSamples[0],mSamples[1],
                 0,GL_RED,GL_UNSIGNED_INT,mData);
    break;
                 }
  case PV_FLOAT:
  case PV_FLOAT32:
    glTexImage2D(sTextureTarget,0,GL_INTENSITY,mSamples[0],mSamples[1],
                 0,GL_RED,GL_FLOAT,mData);
    break;
  case PV_FLOAT64:{
    dataType(PV_FLOAT32);
    
    double* srcd = (double*)mData;
    float* targetf = (float*)mData;
    
    for (int i=0;i<mSamples[0]*mSamples[i];i++)
      targetf[i] = srcd[i];
    
    glTexImage2D(sTextureTarget,0,GL_INTENSITY,mSamples[0],mSamples[1],
                 0,GL_RED,GL_FLOAT,mData);
    break;
                  }
  case PV_RGB:
    glTexImage2D(sTextureTarget,0,GL_RGB,mSamples[0],mSamples[1],
                 0,GL_RGB,GL_UNSIGNED_BYTE,mData);
    break;
  case PV_RGBA:
    glTexImage2D(sTextureTarget,0,GL_RGBA,mSamples[0],mSamples[1],
                 0,GL_RGBA,GL_UNSIGNED_BYTE,mData);
    break;
    
  }  
  
  vglerror();
	
  return 1;
}


int VisusTexture::swapContent(VisusData* data) 
{
  // If I can't read the input I cannot proceed
  if (!readCompatible(data)) {
    vwarning("Data types not compatible cannot swap.");
    return 0;
  }

  // If I can read the input but the input cannot read from me I
  // default to copying the dat
  if (!data->readCompatible(this)) {
    return copyContent(data);
  }  

  //fprintf(stderr,"VisusTexture::swapContent\n");

  // We can use the block data swap for the basic information
  int flag = VisusBlockData::swapContent(data);
  
  // If the data swapping worked we also copy the texture bounds
  VisusTexture* texture = dynamic_cast<VisusTexture*>(data);
  if (texture!=NULL)
  {
    mTexMax[0] = texture->mTexMax[0];
    mTexMax[1] = texture->mTexMax[1];
    mFilename  = texture->mFilename;
  }
  else{
    switch (sTextureTarget) {
    case GL_TEXTURE_RECTANGLE_ARB:
      mTexMax[0] = mSamples[0];
      mTexMax[1] = mSamples[1];
      break;
    case GL_TEXTURE_2D:
      mTexMax[0] = 1;
      mTexMax[1] = 1;
    default:
      vwarning("Texture target not recognized.");
      break;
    }
  }
     
  return flag;
}

int VisusTexture::copyContent(const VisusData* data)
{
  if (!readCompatible(data)) {
    vwarning("Data type not recognized cannot copy.");
    return 0;
  }

  // Carefull, we CANNOT use the VisusBlockData::copyContent(data) for
  // now. VisusBlockData will try to copy mMaskSize many bytes from
  // data into this. However, since this is a texture its mask is potentially
  // eight times bigger than data's which causes a segfault

  const VisusBlockData* block_data = (const VisusBlockData*) data;

  VisusMetricData::copyContent(data); 

  // Now copy all the size independent information
  this->mCellCentered = block_data->cellCentered();
  this->mDataType = block_data->dataType();
  this->mSampleSize = block_data->sampleSize();
  this->mSamples = block_data->samples();
  this->mExtent = block_data->extent();
  block_data->getFieldRange(mMinFieldValue,mMaxFieldValue);
  
  // Now make sure we locally have enough space to copy the data. This
  // is necessary since our own mask array is actually a byte array
  // not a bit array so we might need more space
  reserveSpace();
 
  if (mData != NULL)
    memcpy(mData,block_data->data(),mDataSize);

  // If the source data is block data we must convert the bit array
  // into a byte array. Our reserve() call made enough space. 
  if (data->dataFormat() == VISUS_BLOCK_DATA) {
    for (int i=0;i<mMaskSize;i++) {
      this->mMask[i] = 255 * (1-block_data->mask2(i));
      //fprintf(stderr,"%d ",block_data->mask2(i));
    }
  }

  if (data->dataFormat() == VISUS_TEXTURE_DATA) {
    const VisusTexture* texture = dynamic_cast<const VisusTexture*>(data);
    if (texture!=NULL)
    {
      mTexMax = texture->mTexMax;
      mFilename = texture->mFilename;
    }
    else {
      vwarning("data should be a VISUS_TEXTURE_DATA but dynamic_cast to VisusTexture failed");
    }
  }
  else {
    switch (sTextureTarget) {
    case GL_TEXTURE_RECTANGLE_ARB:
      mTexMax[0] = mSamples[0];
      mTexMax[1] = mSamples[1];
      break;
    case GL_TEXTURE_2D:
      mTexMax[0] = 1;
      mTexMax[1] = 1;
    default:
      vwarning("Texture target not recognized.");
      break;
    }
  }
    
  
  return 1;
}

bool VisusTexture::readCompatible(const VisusData* data) const
{
  if (data == NULL)
    return false;
  
  if ((data->dataFormat() == VISUS_BLOCK_DATA) 
      || (data->dataFormat() == VISUS_TEXTURE_DATA)) {
    return true;
  }
 
  return false;
}


int VisusTexture::reserveSpace()
{
  mMaskSize = usedSamples();
  mDataSize = mMaskSize*mSampleSize;

  //fprintf(stderr,"%d %d %d\n",mMaskSize,mDataSize,mDataCapacity);
    
  if (mDataSize > mDataCapacity) {
    mDataCapacity = mDataSize;
    mData = (unsigned char*)realloc(mData,mDataCapacity);
    
    if ((mData == NULL) && (mDataCapacity > 0)) {
      vwarning("Could not allocate memory of data array. Creating empty data object");
      if (mMaskCapacity > 0) {
        free(mMask);
        mMask = NULL;
      }
      mMaskSize = 0;
      mMaskCapacity = 0;
      mDataSize = 0;
      mDataCapacity = 0;

      mSamples = std::vector<int>(3,0);

      return 0;
    }
  }

  if (mMaskSize > mMaskCapacity) {
    mMaskCapacity = mMaskSize;
    mMask = (unsigned char*)realloc(mMask,mMaskCapacity);
    
    if ((mMask == NULL) && (mMaskCapacity > 0)) {
      vwarning("Could not allocate memory of mask array. Creating empty data object");
      if (mDataCapacity > 0) {
        free(mData);
        mData = NULL;
      }
      mMaskSize = 0;
      mMaskCapacity = 0;
      mDataSize = 0;
      mDataCapacity = 0;
      mSamples = std::vector<int>(3,0);

      return 0;
    }
  }

  return 1;
}



int VisusTexture::compileShader()
{
  GLhandleARB shader;

  shader = glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB);
  glShaderSourceARB(shader,sShaderLength,sShaderCode,NULL);
  glCompileShaderARB(shader);

  sIndirectColorMap = glCreateProgramObjectARB();
  glAttachObjectARB(sIndirectColorMap,shader);
  glLinkProgramARB(sIndirectColorMap);

  char log[500];
  GLsizei count;
  glGetInfoLogARB(sIndirectColorMap,500,&count,log);
  fprintf(stderr,"%s\n",log);
  
  return 1;
}


int VisusTexture::loadPPM(const char *file)
{
  int wid, hgt;

  std::ifstream in(file, std::ios::in|std::ios::binary);
  if (!in) {
    std::cerr << "Error: cannot open " << file << std::endl;
    return 0;
  }
  char header[3];
  in.getline(header, 3);

  int comps = (strcmp(header, "P5") == 0) ? 1 : 3;
  in >> wid >> hgt;

  int val;
  in >> val;
  in.ignore(1024, '\n');

  unsigned int size = comps * wid * hgt;
  unsigned char* data = new unsigned char[size];
    
  in.read((char*)data, size);
  in.close();

  mFilename = file;

  // Connect Up To Read Values
  this->mSamples[0] = wid;
  this->mSamples[1] = hgt;
  this->mSamples[2] = 1;

  // Variable not used ? Removed to avoid warning (ptb)
  //GLenum format = (comps == 1) ? GL_LUMINANCE : GL_RGB;
  PvDataType dataType = (comps == 1 ) ? PV_UCHAR : PV_RGB;

  this->dataType(dataType);
  this->reserveSpace();

  // Flip The Image
  for (int i=0; i<wid; ++i) {
    for (int j=0; j<hgt; ++j) {
      for (int k=0; k<comps; ++k) {
        this->mData[comps*(j*wid + i) + k] = data[comps*((hgt-1-j)*wid + i) + k];
      }
    }
  }


  std::vector<double> ex(3);
  ex[0] = wid;
  ex[1] = hgt;
  ex[2] = 0;
  this->extent(ex);

  for (int i=0; i<wid*hgt; ++i) {
    this->mMask[i] = 255;
  }
     
  switch (sTextureTarget) {
  case GL_TEXTURE_RECTANGLE_ARB:
    mTexMax[0] = mSamples[0];
    mTexMax[1] = mSamples[1];
    break;
  case GL_TEXTURE_2D:
    mTexMax[0] = 1;
    mTexMax[1] = 1;
  default:
    vwarning("Texture target not recognized.");
    break;
  }

  delete[] data;

  return 1;
}

int VisusTexture::extractPeriodicBoundary(int axis, VisusTexture& boundary) const
{
  std::vector<int> samples(3,0);
  std::vector<double> extent(3,0);

  //fprintf(stderr,"VisusTexture::extractPeriodicBoundary  %d %d %d\n",2,mSamples[1],mSampleSize);

  boundary.VisusMetricData::copyContent(this); 

  boundary.mMinFieldValue = this->mMinFieldValue;
  boundary.mMaxFieldValue = this->mMaxFieldValue;
  boundary.mCellCentered = this->mCellCentered;
  boundary.dataType(this->dataType());
  boundary.sampleSize(this->sampleSize());
  boundary.id(this->id());

  if (axis == 0) { // Periodic in X need to save the first and last column

    samples[0] = 2;
    samples[1] = mSamples[1];
    samples[2] = 1;

    boundary.samples(samples);
    boundary.reserveSpace();

    boundary.mTexMax[0] = 2;
    boundary.mTexMax[1] = mSamples[1];
  
    // Copy all the data
    for (int i=0;i<mSamples[1];i++) {
      
      memcpy(boundary.mData+2*i*mSampleSize,    this->mData + ((i+1)*mSamples[0]-1)*mSampleSize,mSampleSize);
      memcpy(boundary.mData+(2*i+1)*mSampleSize,this->mData + i*mSamples[0]*mSampleSize,mSampleSize);
      
      //boundary.mask(2*i,this->mask2((i+1)*mSamples[0]-1));
      //boundary.mask(2*i+1,this->mask2(i*mSamples[0]));
     }
  }
  else if (axis == 1) {
    
    samples[0] = mSamples[0];
    samples[1] = 2;
    samples[2] = 1;

    boundary.samples(samples);
    boundary.reserveSpace();
    
    boundary.mTexMax[0] = mSamples[0];
    boundary.mTexMax[1] = 2;
    
    memcpy(boundary.mData,this->mData,mSampleSize*mSamples[0]);
    memcpy(boundary.mData + mSampleSize*mSamples[0],
           this->mData + mSampleSize*(mSamples[1]-1)*mSamples[0] ,mSampleSize*mSamples[0]);
  }
  else {
    vassert(false);
    return 0;
  }

  return 1;
}


void VisusTexture::toXMLLocalVariables(XMLNode& parent) const
{
  VisusBlockData::toXMLLocalVariables(parent);
  
  parent.addAttribute("filename", mFilename.c_str());

  addChild(parent, "texMax", mTexMax);

  parent.addAttribute("lastId", mLastId);
  parent.addAttribute("texId", (int) mTexId);
  parent.addAttribute("maskId", (int) mMaskId);
}

bool VisusTexture::fromXMLLocalVariables(XMLNode& node, XMLDataStorage storageType)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusTexture did not receive top level node");
    return false;
  } 

  mFilename = node.getAttribute("filename");

  if (! VisusBlockData::fromXMLLocalVariables(node, storageType))
    return false;
  
  getChild(node, "texMax", mTexMax);

  mLastId = xmltoi(node.getAttribute("lastId"), mLastId);
  mTexId = xmltoi(node.getAttribute("texId"), mTexId);
  mMaskId = xmltoi(node.getAttribute("maskId"), mMaskId);

  return true;
}

void VisusTexture::saveXMLData(XMLNode& node) const
{
  if (mFilename.length() <= 0) {
    VisusBlockData::saveXMLData(node);
  }
}

bool VisusTexture::loadXMLData(XMLNode& node, XMLDataStorage storageType)
{
  if (mFilename.length() <= 0) {
    return VisusBlockData::loadXMLData(node, storageType);
  }

  return loadPPM(mFilename.c_str());
}
