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


#ifndef VISUSCAMERA_H
#define VISUSCAMERA_H


#include <vector>

#include "VisusTransformation3D.h"
#include "VisusOpenGLState.h"

struct XMLNode;

enum VisusCameraType {
  VISUS_ORTHOGRAPHIC = 0,
  VISUS_PERSPECTIVE = 1,
};

class VisusCamera 
{
public:


  friend VisusCamera interpolate(const VisusCamera& cam1, const VisusCamera& cam2, float t);
  
  static const char* XML_TAG;

  static const VisusCameraType sCameraTypeDefault;
  static const float sPositionDefault[3];
  static const float sViewDefault[3];
  static const float sUpDefault[3];
  static const float sRightDefault[3];
  static const float sViewAngleDefault;
  static const float sNearPlaneDefault;
  static const float sFarPlaneDefault;
  static const float sRotationSpeedDefault;
  static const float sPanSpeedDefault;
  static const float sZoomFactorDefault;
  static const float sFudgeFactor;

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  

  //! Default constructor
  VisusCamera();

  //! Copy constructor
  VisusCamera(const VisusCamera& camera);

  //! Destructor
  ~VisusCamera();

  //! Assignment operator
  VisusCamera& operator=(const VisusCamera& camera);

  /***************************************************************
   ******          Access Functions                      *********
   **************************************************************/  
  
  //! Return whether the camera is currently active
  bool active() const {return mActive;}

  //! Return the current camera type
  VisusCameraType cameraType() const {return mCameraType;}

  //! Return the current modelview matrix
  VisusTransformation3D modelView() const {return mModelView;}

  //! Return the current viewing angle
  float viewAngle() const {return mViewAngle;}

  //! Return the distance to the near clipping plane
  float nearPlane() const {return mNear;}
  
  //! Return the distance to the far clipping plane
  float farPlane() const {return mFar;}


  //! Mark the camera as active or inactive
  void active(bool a) {mActive = a;}
  
  //! Set the camera type
  void cameraType(VisusCameraType t) {mCameraType = t;}

  //! Set the view angle
  void viewAngle(float angle) {mViewAngle = angle;}

  //! Set the distance to the near clipping plane
  void nearPlane(float n) {mNear = n;}

  //! Set the distance to the far clipping plan
  void farPlane(float f) {mFar = f;}

  //! Set the position of the camera 
  void position(float p[3]);

  //! Set the camera orientation
  int orientation(float view[3], float up[3]);

  //! Set the camera using position view direction and up vector
  int setCamera(float pos[3], float view[3], float up[3]);
  
  //! Build instance data from XML data
  bool fromXML(XMLNode& node);

  //! Build instance data into XML tree
  void toXML(XMLNode& parent) const;

  /***************************************************************
   ******          Camera/Scene Motion                   *********
   **************************************************************/  

  //! Rotate the scene
  void rotate(const VisusTransformation3D& accumulate, float* center, float x, float y);
  
  //! Translate the scene
  void translate(const VisusTransformation3D& accumulate, const VisusOpenGLState& state,
                 float x, float y);
  
  //! Scale the scene
  void scale(float* center, float x, float y);

  //! Rotate the camera
  void turn(float x, float y);

  //! Pan the camera
  void pan(float x, float y);

  //! Zoom in and out
  void zoom(float x, float y);

  //! Setup the OpenGL camera NOTE THE REFERENCE HERE >> THIS MUST BE
  //FIXED LATER PROBABLY BY USING THE OPENGL STACK RATHER THAN
  //MULTIPLYING ON OUR WON
  void setupOpenGL(VisusTransformation3D& model_view_3D);

   /***************************************************************
   ******          File Input/Output                     *********
   **************************************************************/  
  
  //! Write the camera information to the given stream
  void save(FILE* output = stderr);

  //! Read the camera information from the given stream
  int load(FILE* input = stdin);


private:

  //! Flag to indicate whether this camera is used
  bool mActive;

  //! Camera type
  VisusCameraType mCameraType;

  //! The model view matrix
  VisusTransformation3D mModelView;

  //! The eye coordinates
  std::vector<float> mPosition;
  
  //! The current view vector 
  std::vector<float> mView;

  //! The current up vector
  std::vector<float> mUp;

  //! The current right vector
  std::vector<float> mRight;
  
  //! The field-of-view angle
  float mViewAngle;

  //! The distance to the near view plane
  float mNear;

  //! The distance to the far view plane
  float mFar;

  //! Rotation speed
  float mRotationSpeed;

  //! Panning speed
  float mPanSpeed;

  //! Zooming factor
  float mZoomFactor;

  void quaternionMultiply(float u[4], float v[4], float w[4]);

};

//! Return an interpolated camera
/*! Returns a camera between *this camera and the given one. For t=0
 *  the function returns a copy of *this for t=1 a copy of
 *  camera. The function does extrapolate but the results should be
 *  used carefully. ote that the return camera is always active. 
 *  Furthermore, the camera type is determined by (*this)
 *  @param camera: End point of the camera path that should be 
 *                 interpolated
 *  @param t: interpolation factor
 *  @return A camera (1-t)*(*this) + t*camera
 */
VisusCamera interpolate(const VisusCamera& cam1, const VisusCamera& cam2, float t);

#endif

