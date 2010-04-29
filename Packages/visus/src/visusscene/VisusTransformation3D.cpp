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


#include "VisusTransformation3D.h"
#include "VisusTrackball.h"
#include "VisusAssert.h"

#ifdef WIN32
#include <windows.h>
#endif

#include "glew.h"

#include <math.h>
#include <algorithm>   
#include <stdio.h>

#include "VisusQuaternion.h"

#ifndef WIN32
using namespace std;
#endif


VisusTransformation3D::VisusTransformation3D() : VisusTransformation<3>()
{
  mTranslationStyle = ALIGNED_TRANSLATION;
  //mTranslationStyle = FREE_TRANSLATION;
}

VisusTransformation3D::VisusTransformation3D(const VisusTransformation3D& t)
  : VisusTransformation<3>(t)
{
  mTranslationStyle = t.mTranslationStyle;
}

VisusTransformation3D& VisusTransformation3D::operator=(const float mat[4][4])
{
  for (int j=0;j<4;j++) {
    for (int i=0;i<4;i++) {
      mMatrix[4*j + i] = mat[i][j];
    }
  }

  return *this;
}

VisusTransformation3D& VisusTransformation3D::operator=(const VisusTransformation3D& t) 
{
	VisusTransformation<3>::operator=(t);  
	mTranslationStyle = t.mTranslationStyle;
  return *this;
}


void VisusTransformation3D::rotate(const VisusTransformation3D& accumulate, float center[4], float x, float y)
{
  float quat[4];
  float rot[4][4];
  float trans_x[4], trans_y[4], trans_z[4];

  
  //fprintf(stderr,"VisusTransformation3D::rotate\n");
  //fprintf(stderr,"%f %f %f\n",mMatrix[0],mMatrix[4],mMatrix[8]);
  //fprintf(stderr,"%f %f %f\n",mMatrix[1],mMatrix[5],mMatrix[9]);
  //fprintf(stderr,"%f %f %f\n",mMatrix[2],mMatrix[6],mMatrix[10]);

  // Compute a quaternion describing the desired rotation in 
  // world/screen space 
  trackball(quat,0,0,x,y);

  // Invert the rotation
  quat[0] *= -1;
  quat[1] *= -1;
  quat[2] *= -1;

  // Transform the quaternion into matrix form
  build_rotmatrix(rot,quat);

  // We want to rotate around the given center. To this end it is
  // easiest to imaging the current transformation matrix to be given
  // as T*R*T^ where T and T^ are the translations to and from the
  // center and R the rotation/scaling matrix. Since we only want to
  // effect the rotation part here we first right multiply with T to
  // "expose" the rotation matrix
  *this *= translationMatrix(center[0],center[1],center[2]);

  // Now we want to figure out how to modify the local coordinate
  // system (given by mMatrix) to achieve the same effect as rotating the
  // world space by "local_rotation".

  // The basis vectors of the current coordinate system are given as
  // the column vectors of mMatrix. Take each one and map it into
  // world coordinates (mapping does not include the translation which
  // we don't care about here).
  accumulate.map(mMatrix, trans_x);
  accumulate.map(mMatrix+4, trans_y);  
  accumulate.map(mMatrix+8, trans_z);

  /*
  fprintf(stderr,"Local x-axis <%f,%f,%f>\n",trans_x[0],trans_x[1],trans_x[2]);
  fprintf(stderr,"Local y-axis <%f,%f,%f>\n",trans_y[0],trans_y[1],trans_y[2]);
  fprintf(stderr,"Local z-axis <%f,%f,%f>\n\n",trans_z[0],trans_z[1],trans_z[2]);
  */

  matmult(trans_x,trans_x,rot);  
  matmult(trans_y,trans_y,rot);
  matmult(trans_z,trans_z,rot);
  
  // Compute the inverse mapping 
  VisusTransformation3D inv = accumulate.inverseMap();

  // Transform the new basis vectors back into local space which
  // defines the new local transformation matrix
  inv.map(trans_x,mMatrix);
  inv.map(trans_y,mMatrix+4);
  inv.map(trans_z,mMatrix+8);

  // Now add the translation to the center again to get the new matrix
  *this *= translationMatrix(-center[0],-center[1],-center[2]);

  // Here we try our best to orthogonalize the matrix. The assumption
  // is that the matrix should never describe a shear but only a pure
  // scaling and rotation
  float mag,factor,factor2;

  mag = mMatrix[0]*mMatrix[0] + mMatrix[1]*mMatrix[1] + mMatrix[2]*mMatrix[2];
  factor = mMatrix[0]*mMatrix[4] + mMatrix[1]*mMatrix[5] + mMatrix[2]*mMatrix[6];
  factor /= mag;

  mMatrix[4] -= factor*mMatrix[0];
  mMatrix[5] -= factor*mMatrix[1];
  mMatrix[6] -= factor*mMatrix[2];
  
  factor = mMatrix[0]*mMatrix[8] + mMatrix[1]*mMatrix[9] + mMatrix[2]*mMatrix[10];
  factor /= mag;

  mag = mMatrix[4]*mMatrix[4] + mMatrix[5]*mMatrix[5] + mMatrix[6]*mMatrix[6];
  factor2 = mMatrix[4]*mMatrix[8] + mMatrix[5]*mMatrix[9] + mMatrix[6]*mMatrix[10];
  factor2 /= mag;

  mMatrix[8] -= factor*mMatrix[0] + factor2*mMatrix[4];
  mMatrix[9] -= factor*mMatrix[1] + factor2*mMatrix[5];
  mMatrix[10] -= factor*mMatrix[2] + factor2*mMatrix[6];
  
  //fprintf(stderr,"x*y %f\n",mMatrix[0]*mMatrix[4]+mMatrix[1]*mMatrix[5]+mMatrix[2]*mMatrix[6]);
  //fprintf(stderr,"x*z %f\n",mMatrix[0]*mMatrix[8]+mMatrix[1]*mMatrix[9]+mMatrix[2]*mMatrix[10]);
  //fprintf(stderr,"y*z %f\n\n",mMatrix[4]*mMatrix[8]+mMatrix[5]*mMatrix[9]+mMatrix[6]*mMatrix[10]);

}

void VisusTransformation3D::translate(const VisusTransformation3D& accumulate, 
                                      const VisusOpenGLState & state, float x, float y)
{
  //fprintf(stderr,"VisusTransformation3D::translate  x=%f  y=%f  style=%d\n",x,y,mTranslationStyle);
  //fflush(stderr);
  switch (mTranslationStyle) {
  case FREE_TRANSLATION:
    translateFreely(accumulate, state, x, y);
    break;
  case ALIGNED_TRANSLATION:
    translateAligned(accumulate, state, x, y);
    break;
  }
}

std::vector<float> VisusTransformation3D::translation(const VisusTransformation3D& accumulate, 
                                                      const VisusOpenGLState & state, float x, float y)
{
  switch (mTranslationStyle) {
  case FREE_TRANSLATION:
    return freeTranslation(accumulate, state, x, y);
    break;
  case ALIGNED_TRANSLATION:
    return alignedTranslation(accumulate, state, x, y);
    break;
  }

  return std::vector<float>(3,0);
}

std::vector<float> VisusTransformation3D::freeTranslation(const VisusTransformation3D& accumulate, 
                                                          const VisusOpenGLState & state, float x, float y)
{
  std::vector<float> t(3);

  GLint const *viewport;  // Current viewport
  GLdouble modelview[16]; // GL Modelview matrix to get us into local coordinates
  GLdouble const *projection; // Current projection matrix 
  GLdouble origin[3];
  GLdouble vx[3],vy[3],px[3],py[3],scale[3];

  //fprintf(stderr,"VisusTransformation3D::translateFreely  x=%f  y=%f\n",x,y);

  // Collect all information about the current gl state
  viewport = state.viewport();
  projection = state.projection();


  //glGetIntegerv(GL_VIEWPORT,viewport);
  //glGetDoublev(GL_PROJECTION_MATRIX,projection);

  // Only copy the first 12 values (the rotation part) 
  for (int i=0;i<12;i++) 
    modelview[i] = accumulate[i];
  modelview[12] = modelview[13] = modelview[14] = 0;
  modelview[15] = 1;

  for (int i=0;i<3;i++) {
    scale[i] = sqrt(modelview[4*i]*modelview[4*i] 
                    + modelview[4*i+1]*modelview[4*i+1] 
                    + modelview[4*i+2]*modelview[4*i+2]);
  }

  for (int j=0;j<3;j++) {
    for (int i=0;i<3;i++) {
      modelview[4*j + i] /= scale[i];
    }
  }

  // Adjust our speed
  x *= mTranslationSpeed;
  y *= mTranslationSpeed;

  gluUnProject(0.5*viewport[2],0.5*viewport[3],0,modelview, projection, viewport,
               origin,origin+1,origin+2);

  gluUnProject(0.5*viewport[2]+1,0.5*viewport[3],0,modelview, projection, viewport,
               px,px+1,px+2);

  gluUnProject(0.5*viewport[2],0.5*viewport[3]+1,0,modelview, projection, viewport,
               py,py+1,py+2);

  for (int i=0;i<3;i++) {
    vx[i] = px[i] - origin[i];
    vy[i] = py[i] - origin[i];
  }
 
  scale[0] = sqrt(vx[0]*vx[0] + vx[1]*vx[1] + vx[2]*vx[2]);
  scale[1] = sqrt(vy[0]*vy[0] + vy[1]*vy[1] + vy[2]*vy[2]);

  for (int i=0;i<3;i++) {
    vx[i] /= scale[0];
    vy[i] /= scale[1];
  }

  t[0] = vx[0]*x + vy[0]*y;
  t[1] = vx[1]*x + vy[1]*y;
  t[2] = vx[2]*x + vy[2]*y;

  return t;
}

/* Helper Function To Compute The Viewing Distance Btw Two Points Projected */
double getViewingDistance(GLint const *viewport, GLdouble modelview[], GLdouble const *projection,
                          double inX1, double inY1, double inZ1,
                          double inX2, double inY2, double inZ2)
{
  double x1,y1,z1,x2,y2,z2;

  gluProject(inX1,inY1,inZ1,modelview,projection,viewport,&x1,&y1,&z1);
  gluProject(inX2,inY2,inZ2,modelview,projection,viewport,&x2,&y2,&z2);

  double dx = x2 - x1;
  double dy = y2 - y1;
  double dz = z2 - z1;

  //double distance = dz;
  double distance = sqrt(dx*dx + dy*dy + dz*dz);
  return distance;
}

std::vector<float> VisusTransformation3D::alignedTranslation(const VisusTransformation3D& accumulate, 
                                                             const VisusOpenGLState & state, float x, float y)
{
  
  //printf("Input matrix: %f %f %f %f\n", mMatrix[0], mMatrix[1], mMatrix[2], mMatrix[3]);
  //printf("              %f %f %f %f\n", mMatrix[4], mMatrix[5], mMatrix[6], mMatrix[7]);
  //printf("              %f %f %f %f\n", mMatrix[8], mMatrix[9], mMatrix[10], mMatrix[11]);
  //printf("              %f %f %f %f\n", mMatrix[12], mMatrix[13], mMatrix[14], mMatrix[15]);

  // Collect all information about the current gl state
  GLint const*viewport;       // Current viewport
  GLdouble modelview[16];  // GL Modelview matrix to get us into local coordinates
  GLdouble const *projection; // Current projection matrix 
  std::vector<float> t(3);
  double scale[3];

  viewport = state.viewport();
  projection = state.projection();

  //glGetIntegerv(GL_VIEWPORT,viewport);  
  //glGetDoublev(GL_PROJECTION_MATRIX,projection);
  

  for (int i=0;i<12;i++) 
    modelview[i] = accumulate[i];
  modelview[12] = modelview[13] = modelview[14] = 0;
  modelview[15] = 1;

  for (int i=0;i<3;i++) {
    scale[i] = sqrt(modelview[4*i]*modelview[4*i] 
                    + modelview[4*i+1]*modelview[4*i+1] 
                    + modelview[4*i+2]*modelview[4*i+2]);
  }

  for (int j=0;j<3;j++) {
    for (int i=0;i<3;i++) {
      modelview[4*j + i] /= scale[j];
    }
  }

  // Adjust our speed
  //x *= mTranslationSpeed;
  //y *= mTranslationSpeed;

  // Determine Most-Perpendicular Slice To View Direction

  // Okay, Theory below is that given a perfect box, the two dimensions that
  // are the longest will represent the most-perpendicular plane to view direction
  VisusSliceOrientation perpView=VISUS_ORIENTATION_XY;

  double dx = getViewingDistance(viewport,modelview,projection,-1.0,-1.0,0.0,1.0,-1.0,0.0);
  double dy = getViewingDistance(viewport,modelview,projection,-1.0,-1.0,0.0,-1.0,1.0,0.0);
  double dz = getViewingDistance(viewport,modelview,projection,-1.0,0,-1.0,-1.0,0,1.0);

  if (dy > dx && dz > dx) {
    perpView = VISUS_ORIENTATION_YZ;
  }
  else if (dx > dy && dz > dy) {
    perpView = VISUS_ORIENTATION_XZ;
  }

  GLdouble origin[3];
  GLdouble vx[3],vy[3],px[3],py[3];

  gluUnProject(0.5*viewport[2],0.5*viewport[3],0,modelview, projection, viewport,
               origin,origin+1,origin+2);

  gluUnProject(0.5*viewport[2]+1,0.5*viewport[3],0,modelview, projection, viewport,
               px,px+1,px+2);

  gluUnProject(0.5*viewport[2],0.5*viewport[3]+1,0,modelview, projection, viewport,
               py,py+1,py+2);

  for (int i=0;i<3;i++) {
    vx[i] = px[i] - origin[i];
    vy[i] = py[i] - origin[i];
  }

  scale[0] = sqrt(vx[0]*vx[0] + vx[1]*vx[1] + vx[2]*vx[2]);
  scale[1] = sqrt(vy[0]*vy[0] + vy[1]*vy[1] + vy[2]*vy[2]);

  for (int i=0;i<3;i++) {
    vx[i] /= scale[0];
    vy[i] /= scale[1];
  }

  // Translate Based On Most-Perpendicular View
  switch(perpView) {
  case VISUS_ORIENTATION_XY:
    t[0] = vx[0]*x + vy[0]*y;
    t[1] = vx[1]*x + vy[1]*y;
    t[2] = 0;
    //printf("XY is longer!\n");
    break;
  case VISUS_ORIENTATION_XZ:
    t[0] = vx[0]*x + vy[0]*y;
    t[1] = 0;
    t[2] = vx[2]*x + vy[2]*y;
    //printf("XZ is longer!\n");
    break;
  case VISUS_ORIENTATION_YZ:
    t[0] = 0;
    t[1] = vx[1]*x + vy[1]*y;
    t[2] = vx[2]*x + vy[2]*y;
    //printf("YZ is longer!\n");
    break;
  }

  //printf("Output matrix: %f %f %f %f\n", mMatrix[0], mMatrix[1], mMatrix[2], mMatrix[3]);
  //printf("               %f %f %f %f\n", mMatrix[4], mMatrix[5], mMatrix[6], mMatrix[7]);
  //printf("               %f %f %f %f\n", mMatrix[8], mMatrix[9], mMatrix[10], mMatrix[11]);
  //printf("               %f %f %f %f\n", mMatrix[12], mMatrix[13], mMatrix[14], mMatrix[15]);

  return t;
}
  

void VisusTransformation3D::translateFreely(const VisusTransformation3D& accumulate, 
                                            const VisusOpenGLState & state, float x, float y)
{
  GLint const* viewport;  // Current viewport
  GLdouble modelview[16]; // GL Modelview matrix to get us into local coordinates
  GLdouble const *projection; // Current projection matrix 
  GLdouble origin[3];
  GLdouble vx[3],vy[3],px[3],py[3];

  //fprintf(stderr,"VisusTransformation3D::translateFreely  x=%f  y=%f\n",x,y);

  // Collect all information about the current gl state

  viewport = state.viewport();
  projection = state.projection();

  //glGetIntegerv(GL_VIEWPORT,viewport);
  //glGetDoublev(GL_PROJECTION_MATRIX,projection);
  
  //fprintf(stderr,"viewport = %d %d %d %d\n",viewport[0],viewport[1],viewport[2],viewport[3]);
  //fprintf(stderr,"projection\n%f %f %f %f\n%f %f %f %f\n%f %f %f %f\n%f %f %f %f\n\n",
  //        projection[0],projection[4],projection[8],projection[12],
  //        projection[1],projection[5],projection[9],projection[13],
  //        projection[2],projection[6],projection[10],projection[14],
  //        projection[3],projection[7],projection[11],projection[15]);

  for (int i=0;i<16;i++) 
    modelview[i] = accumulate.mMatrix[i];

  // Adjust our speed
  x *= mTranslationSpeed;
  y *= mTranslationSpeed;

  gluUnProject(0.5*viewport[2],0.5*viewport[3],0,modelview, projection, viewport,
               origin,origin+1,origin+2);

  gluUnProject(0.5*viewport[2]+1,0.5*viewport[3],0,modelview, projection, viewport,
               px,px+1,px+2);

  gluUnProject(0.5*viewport[2],0.5*viewport[3]+1,0,modelview, projection, viewport,
               py,py+1,py+2);

  for (int i=0;i<3;i++) {
    vx[i] = px[i] - origin[i];
    vy[i] = py[i] - origin[i];
  }

  mMatrix[12] += vx[0]*x + vy[0]*y;
  mMatrix[13] += vx[1]*x + vy[1]*y;
  mMatrix[14] += vx[2]*x + vy[2]*y;

}

void VisusTransformation3D::translateAligned(const VisusTransformation3D& accumulate, 
                                             const VisusOpenGLState & state, float x, float y)
{ 
  //printf("Input matrix: %f %f %f %f\n", mMatrix[0], mMatrix[1], mMatrix[2], mMatrix[3]);
  //printf("              %f %f %f %f\n", mMatrix[4], mMatrix[5], mMatrix[6], mMatrix[7]);
  //printf("              %f %f %f %f\n", mMatrix[8], mMatrix[9], mMatrix[10], mMatrix[11]);
  //printf("              %f %f %f %f\n", mMatrix[12], mMatrix[13], mMatrix[14], mMatrix[15]);

  // Collect all information about the current gl state
  GLint const *viewport;       // Current viewport
  GLdouble modelview[16];  // GL Modelview matrix to get us into local coordinates
  GLdouble const *projection; // Current projection matrix 

  viewport = state.viewport();
  projection = state.projection();

  //fprintf(stderr,"viewport = %d %d %d %d\n",viewport[0],viewport[1],viewport[2],viewport[3]);
  //fprintf(stderr,"projection\n%f %f %f %f\n%f %f %f %f\n%f %f %f %f\n%f %f %f %f\n\n",
  //        projection[0],projection[4],projection[8],projection[12],
  //        projection[1],projection[5],projection[9],projection[13],
  //        projection[2],projection[6],projection[10],projection[14],
  //        projection[3],projection[7],projection[11],projection[15]);
  //glGetIntegerv(GL_VIEWPORT,viewport);  
  //glGetDoublev(GL_PROJECTION_MATRIX,projection);
  for (int i=0;i<16;i++) 
    modelview[i] = accumulate.mMatrix[i];

  // Adjust our speed
  x *= mTranslationSpeed;
  y *= mTranslationSpeed;

  // Determine Most-Perpendicular Slice To View Direction

  // Okay, Theory below is that given a perfect box, the two dimensions that
  // are the longest will represent the most-perpendicular plane to view direction
  VisusSliceOrientation perpView=VISUS_ORIENTATION_XY;

  double dx = getViewingDistance(viewport,modelview,projection,-1.0,-1.0,0.0,1.0,-1.0,0.0);
  double dy = getViewingDistance(viewport,modelview,projection,-1.0,-1.0,0.0,-1.0,1.0,0.0);
  double dz = getViewingDistance(viewport,modelview,projection,-1.0,0,-1.0,-1.0,0,1.0);

  if (dy > dx && dz > dx) {
    perpView = VISUS_ORIENTATION_YZ;
  }
  else if (dx > dy && dz > dy) {
    perpView = VISUS_ORIENTATION_XZ;
  }

  GLdouble origin[3];
  GLdouble vx[3],vy[3],px[3],py[3];

  gluUnProject(0.5*viewport[2],0.5*viewport[3],0,modelview, projection, viewport,
               origin,origin+1,origin+2);

  gluUnProject(0.5*viewport[2]+1,0.5*viewport[3],0,modelview, projection, viewport,
               px,px+1,px+2);

  gluUnProject(0.5*viewport[2],0.5*viewport[3]+1,0,modelview, projection, viewport,
               py,py+1,py+2);

  for (int i=0;i<3;i++) {
    vx[i] = px[i] - origin[i];
    vy[i] = py[i] - origin[i];
  }

  // Translate Based On Most-Perpendicular View
  switch(perpView) {
  case VISUS_ORIENTATION_XY:
    mMatrix[12] += vx[0]*x + vy[0]*y;
    mMatrix[13] += vx[1]*x + vy[1]*y;
    //printf("XY is longer!\n");
    break;
  case VISUS_ORIENTATION_XZ:
    mMatrix[12] += vx[0]*x + vy[0]*y;
    mMatrix[14] += vx[2]*x + vy[2]*y;
    //printf("XZ is longer!\n");
    break;
  case VISUS_ORIENTATION_YZ:
    mMatrix[13] += vx[1]*x + vy[1]*y;
    mMatrix[14] += vx[2]*x + vy[2]*y;
    //printf("YZ is longer!\n");
    break;
  }

  //printf("Output matrix: %f %f %f %f\n", mMatrix[0], mMatrix[1], mMatrix[2], mMatrix[3]);
  //printf("               %f %f %f %f\n", mMatrix[4], mMatrix[5], mMatrix[6], mMatrix[7]);
  //printf("               %f %f %f %f\n", mMatrix[8], mMatrix[9], mMatrix[10], mMatrix[11]);
  //printf("               %f %f %f %f\n", mMatrix[12], mMatrix[13], mMatrix[14], mMatrix[15]);

}

void VisusTransformation3D::scale(float* center, float x, float y)
{
  float factor;

  factor = 1 + y*mScalingSpeed;
  
  *this *= translationMatrix(center[0],center[1],center[2]);
  *this *= scaleMatrix(factor,factor,factor);
  *this *= translationMatrix(-center[0],-center[1],-center[2]);  
}


void VisusTransformation3D::normalize()
{
  float mag;

  for (int i=0;i<3;i++) {
    mag = mMatrix[4*i+0]*mMatrix[4*i+0] +  mMatrix[4*i+1]*mMatrix[4*i+1]  + mMatrix[4*i+2]*mMatrix[4*i+2];
    mag = sqrt(mag);

    mMatrix[4*i+0] /= mag;
    mMatrix[4*i+1] /= mag;
    mMatrix[4*i+2] /= mag;
  }
}

VisusTransformation3D VisusTransformation3D::inverseMap() const
{
  float det;    // Determinant of the non-translation part of the matrix
  VisusTransformation3D inv;

  det = (+ mMatrix[0]*(mMatrix[10]*mMatrix[5] - mMatrix[6]*mMatrix[9])
         - mMatrix[1]*(mMatrix[10]*mMatrix[4] - mMatrix[6]*mMatrix[8])
         + mMatrix[2]*(mMatrix[ 4]*mMatrix[9] - mMatrix[8]*mMatrix[5]));
    
  // Compute the length largest column vector of the matrix to determine whether
  // it is degenerate
  float mag[3];
  mag[0] = mMatrix[0]*mMatrix[0] + mMatrix[1]*mMatrix[1] + mMatrix[2]*mMatrix[2];  
  mag[1] = mMatrix[4]*mMatrix[4] + mMatrix[5]*mMatrix[5] + mMatrix[6]*mMatrix[6];   
  mag[2] = mMatrix[8]*mMatrix[8] + mMatrix[9]*mMatrix[9] + mMatrix[10]*mMatrix[10]; 
  
  mag[0] = std::max(mag[0],std::max(mag[1],mag[2]));
  if (fabs(det) < sFudgeFactor*mag[0]) {
    vwarning("3D Transformation matrix cannot be inverted ... returning ID");
    return inv;
  }


  inv[0]  =   (mMatrix[10]*mMatrix[5] - mMatrix[6]*mMatrix[9]) / det;
  inv[1]  = - (mMatrix[10]*mMatrix[1] - mMatrix[2]*mMatrix[9]) / det;
  inv[2]  =   (mMatrix[ 6]*mMatrix[1] - mMatrix[2]*mMatrix[5]) / det;

  inv[4]  = - (mMatrix[10]*mMatrix[4] - mMatrix[6]*mMatrix[8]) / det;
  inv[5]  =   (mMatrix[10]*mMatrix[0] - mMatrix[2]*mMatrix[8]) / det;
  inv[6]  = - (mMatrix[ 6]*mMatrix[0] - mMatrix[2]*mMatrix[4]) / det;
  
  inv[8]  =   (mMatrix[ 9]*mMatrix[4] - mMatrix[5]*mMatrix[8]) / det;
  inv[9]  = - (mMatrix[ 9]*mMatrix[0] - mMatrix[1]*mMatrix[8]) / det;
  inv[10] =   (mMatrix[ 5]*mMatrix[0] - mMatrix[1]*mMatrix[4]) / det;

  inv[15] = 1;
  
  return inv;
}

VisusTransformation3D VisusTransformation3D::inverseTransform() const
{
  float det;    // Determinant of the non-translation part of the matrix
  VisusTransformation3D inv;

  det = (+ mMatrix[0]*(mMatrix[10]*mMatrix[5] - mMatrix[6]*mMatrix[9])
         - mMatrix[1]*(mMatrix[10]*mMatrix[4] - mMatrix[6]*mMatrix[8])
         + mMatrix[3]*(mMatrix[ 4]*mMatrix[9] - mMatrix[8]*mMatrix[5]));
    
  if (fabs(det) < sFudgeFactor) {
    vwarning("3D Transformation matrix cannot be inverted ... returning ID");
    return inv;
  }


  inv[0]  =   (mMatrix[10]*mMatrix[5] - mMatrix[6]*mMatrix[9]) / det;
  inv[1]  = - (mMatrix[10]*mMatrix[1] - mMatrix[2]*mMatrix[9]) / det;
  inv[2]  =   (mMatrix[ 6]*mMatrix[1] - mMatrix[2]*mMatrix[5]) / det;
  
  inv[4]  = - (mMatrix[10]*mMatrix[4] - mMatrix[6]*mMatrix[8]) / det;
  inv[5]  =   (mMatrix[10]*mMatrix[0] - mMatrix[2]*mMatrix[8]) / det;
  inv[6]  = - (mMatrix[ 6]*mMatrix[0] - mMatrix[2]*mMatrix[4]) / det;
  
  inv[8]  =   (mMatrix[ 9]*mMatrix[4] - mMatrix[5]*mMatrix[8]) / det;
  inv[9]  = - (mMatrix[ 9]*mMatrix[0] - mMatrix[1]*mMatrix[8]) / det;
  inv[10] =   (mMatrix[ 5]*mMatrix[0] - mMatrix[1]*mMatrix[4]) / det;
  
  // The new translation is the negative of the old translation
  // transformed by the inverse of the rotation
  for (int i=0;i<3;i++) 
    inv[12+i] = -inv[i]*mMatrix[12] - inv[3+i]*mMatrix[13] - inv[6+i]*mMatrix[14];

  return inv;
}

void VisusTransformation3D::print()
{ 
  fprintf(stderr,"%.8f %.8f %.8f %.8f\n",mMatrix[0],mMatrix[4],mMatrix[8],mMatrix[12]);
  fprintf(stderr,"%.8f %.8f %.8f %.8f\n",mMatrix[1],mMatrix[5],mMatrix[9],mMatrix[13]);
  fprintf(stderr,"%.8f %.8f %.8f %.8f\n",mMatrix[2],mMatrix[6],mMatrix[10],mMatrix[14]);
  fprintf(stderr,"%.8f %.8f %.8f %.8f\n",mMatrix[3],mMatrix[7],mMatrix[11],mMatrix[15]);
}


int VisusTransformation3D::save(FILE* output)
{
  if (output == NULL) {
    vwarning("Cannot save VisusTransformation3D to NULL file.");
    return 0;
  }
  
  fprintf(output,"<VisusTransformation3D>\n");
  fprintf(output,"%.8f %.8f %.8f %.8f\n",mMatrix[0],mMatrix[4],mMatrix[8],mMatrix[12]);
  fprintf(output,"%.8f %.8f %.8f %.8f\n",mMatrix[1],mMatrix[5],mMatrix[9],mMatrix[13]);
  fprintf(output,"%.8f %.8f %.8f %.8f\n",mMatrix[2],mMatrix[6],mMatrix[10],mMatrix[14]);
  fprintf(output,"%.8f %.8f %.8f %.8f\n",mMatrix[3],mMatrix[7],mMatrix[11],mMatrix[15]);
  fprintf(output,"<VisusTransformation3D>\n");

  return 1;
}

int VisusTransformation3D::load(FILE* input)
{
  if (input == NULL) {
    vwarning("Cannot load VisusTransformation3D from NULL file.");
    return 0;
  }

  char token[100];

  fscanf(input,"%s",token);
  if (strcmp(token,"<VisusTransformation3D>") != 0) {
    vwarning("Incorrect token cannot read in VisusTransformation3D");
    return 0;
  }

  fscanf(input,"%f %f %f %f",&(mMatrix[0]),&(mMatrix[4]),&(mMatrix[8]),&(mMatrix[12]));
  fscanf(input,"%f %f %f %f",&(mMatrix[1]),&(mMatrix[5]),&(mMatrix[9]),&(mMatrix[13]));
  fscanf(input,"%f %f %f %f",&(mMatrix[2]),&(mMatrix[6]),&(mMatrix[10]),&(mMatrix[14]));
  fscanf(input,"%f %f %f %f",&(mMatrix[3]),&(mMatrix[7]),&(mMatrix[11]),&(mMatrix[15]));
  
  
  fscanf(input,"%s",token);
  if (strcmp(token,"<VisusTransformation3D>") != 0) {
    fprintf(stderr,"token \"%s\"\n",token);
    vwarning("VisusTransformation3D file input not consistent.");
    return 0;
  }
  
  
  return 1;
}
  

VisusTransformation3D operator*(const VisusTransformation3D& t1,
                                const VisusTransformation3D& t2)
{
  VisusTransformation3D result;
  for (int row=0;row<=3;row++) {
    for (int column=0;column<=3;column++) {
      result[column*4 + row] = 0;
      for (int i=0;i<4;i++) {
        result[column*4 + row] += t1.mMatrix[i*4 + row]*t2.mMatrix[column*4 + i];
      }
    }
  }

  return result;
}

VisusTransformation3D translationMatrix(float x, float y, float z)
{
  VisusTransformation3D matrix;

  matrix[12] = x;
  matrix[13] = y;
  matrix[14] = z;  

  return matrix;
}


VisusTransformation3D scaleMatrix(float sx, float sy, float sz)
{
  VisusTransformation3D matrix;

  matrix[0] = sx;
  matrix[5] = sy;
  matrix[10] = sz;  

  return matrix;  
}

VisusTransformation3D xyPlane()
{
  return VisusTransformation3D();
}

VisusTransformation3D xzPlane()
{
  VisusTransformation3D matrix;

  matrix[5] = 0;
  matrix[6] = 1;

  matrix[9] = -1;
  matrix[10] = 0;

  return matrix;
}

VisusTransformation3D yzPlane()
{
  VisusTransformation3D matrix;

  matrix[0] = 0;
  matrix[1] = 1;

  matrix[5] = 0;
  matrix[6] = 1;

  matrix[8] = 1;
  matrix[10] = 0;

  return matrix;
}



VisusTransformation3D slerp(VisusTransformation3D t1,VisusTransformation3D t2, float t)
{
  VisusTransformation3D result;
  VisusQuaternion q1,q2;
  float s1[3];
  float s2[3];
  

  s1[0] = sqrt(t1[0]*t1[0] + t1[1]*t1[1] + t1[2]*t1[2]); 
  s1[1] = sqrt(t1[4]*t1[4] + t1[5]*t1[5] + t1[6]*t1[6]); 
  s1[2] = sqrt(t1[8]*t1[8] + t1[9]*t1[9] + t1[10]*t1[10]); 

  t1[0] /= s1[0];t1[1] /= s1[0];t1[2] /= s1[0];
  t1[4] /= s1[1];t1[5] /= s1[1];t1[6] /= s1[1];
  t1[8] /= s1[2];t1[9] /= s1[2];t1[10] /= s1[2];

  s2[0] = sqrt(t2[0]*t2[0] + t2[1]*t2[1] + t2[2]*t2[2]); 
  s2[1] = sqrt(t2[4]*t2[4] + t2[5]*t2[5] + t2[6]*t2[6]); 
  s2[2] = sqrt(t2[8]*t2[8] + t2[9]*t2[9] + t2[10]*t2[10]); 

  t2[0] /= s2[0];t2[1] /= s2[0];t2[2] /= s2[0];
  t2[4] /= s2[1];t2[5] /= s2[1];t2[6] /= s2[1];
  t2[8] /= s2[2];t2[9] /= s2[2];t2[10] /= s2[2];

  
  q1.fromMatrix(t1);
  q2.fromMatrix(t2);
  

  q1 = slerp(q1,q2,t);
  
  s1[0] = (1-t)*s1[0] + t*s2[0];
  s1[1] = (1-t)*s1[1] + t*s2[1];
  s1[2] = (1-t)*s1[2] + t*s2[2];

  q1.toMatrix(result);
  
  result[0] *= s1[0]; result[1] *= s1[0]; result[2] *= s1[0];
  result[4] *= s1[1]; result[5] *= s1[1]; result[6] *= s1[1];
  result[8] *= s1[2]; result[9] *= s1[2]; result[10] *= s1[2];

  for (int i=12;i<15;i++)
    result[i] = (1-t)*t1[i] + t*t2[i];

  return result;
}
