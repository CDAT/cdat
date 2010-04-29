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


#ifdef WIN32
#include <windows.h>
#endif

#include "glew.h"
#include <math.h>

#include "xmlParser.h"

#include "VisusCamera.h"
#include "VisusAssert.h"
#include "VisusQuaternion.h"
#include "VisusTransformation3D.h"

const char* VisusCamera::XML_TAG = "VisusCamera";
const VisusCameraType VisusCamera::sCameraTypeDefault = VISUS_PERSPECTIVE;
//const VisusCameraType VisusCamera::sCameraTypeDefault = VISUS_ORTHOGRAPHIC;
const float VisusCamera::sPositionDefault[3] = {0,0,65};
const float VisusCamera::sViewDefault[3] = {0,0,-1};
const float VisusCamera::sUpDefault[3] = {0,1,0};
const float VisusCamera::sRightDefault[3] = {1,0,0};
const float VisusCamera::sViewAngleDefault = 15;
const float VisusCamera::sNearPlaneDefault = 10;
const float VisusCamera::sFarPlaneDefault = 1000;
const float VisusCamera::sFudgeFactor = 10;
const float VisusCamera::sRotationSpeedDefault = 3;
const float VisusCamera::sPanSpeedDefault = 6;
const float VisusCamera::sZoomFactorDefault = 0.01;

VisusCamera::VisusCamera() : mPosition(3), mView(3), mUp(3), mRight(3)
{
  mActive = true;
  mCameraType = sCameraTypeDefault;

  for (int i=0;i<3;i++) {
    
    mPosition[i] = sPositionDefault[i];
    mView[i] = sViewDefault[i];
    mUp[i] = sUpDefault[i];
    mRight[i] = sRightDefault[i];
  }
 
  mViewAngle = sViewAngleDefault;
  mNear = sNearPlaneDefault;
  mFar = sFarPlaneDefault;
  
  mRotationSpeed = sRotationSpeedDefault;
  mPanSpeed = sPanSpeedDefault;
  mZoomFactor = sZoomFactorDefault;
}

VisusCamera::VisusCamera(const VisusCamera& camera)
{
  *this = camera;
}

VisusCamera::~VisusCamera()
{
}

VisusCamera& VisusCamera::operator=(const VisusCamera& camera)
{
  mActive = camera.mActive;
  mCameraType = camera.mCameraType;

  mPosition = camera.mPosition;
  mView = camera.mView;
  mUp = camera.mUp;
  mRight = camera.mRight;
  

  mViewAngle = camera.mViewAngle;
  mNear = camera.mNear;
  mFar = camera.mFar;
  
  mRotationSpeed = camera.mRotationSpeed;
  mPanSpeed = camera.mPanSpeed;
  mZoomFactor = camera.mZoomFactor;
 

  mModelView = camera.mModelView;

  return *this;
}

void VisusCamera::position(float p[3])
{
  mPosition[0] = p[0];  
  mPosition[1] = p[1];  
  mPosition[2] = p[2];
}

int VisusCamera::orientation(float view[3], float up[3])
{
  float mag;

  // First we determine whether the view vector as a proper magnitude 
  mag = sqrt(view[0]*view[0] + view[1]*view[1] + view[2]*view[2]);

  // If the magnitude is too small we cannot correctly derive a
  // direction from it and therefore reject the change
  if (mag < sFudgeFactor) {
    vwarning("View vector close to zero. Orienation not modified.");
    return 0;
  }

  // Otherwise we scale it to get a unti vector
  view[0] /= mag;
  view[1] /= mag;
  view[2] /= mag;

  //fprintf(stderr,"New view <%f,%f,%f>\n",view[0],view[1],view[2]);

  // Now determined whether the up vector has a decent magnitude
  mag = sqrt(up[0]*up[0] + up[1]*up[1] + up[2]*up[2]);

  // If no we reject the change
  if (mag < sFudgeFactor) {
    vwarning("Up vector close to zero. Orientation not modified.");
    return 0;
  }
  
  // Scale the up vector to become unit length
  up[0] /= mag;
  up[1] /= mag;
  up[2] /= mag;

  // Check whether the up vector is parallel to the view vector
  mag = view[0]*up[0] + view[1]*up[1] + view[2]*up[2];

  // If the angle is too close to 0 or 180 degrees (not we check the
  // cosine here) we reject the change
  if ((mag > (1-sFudgeFactor)) || (mag < (sFudgeFactor - 1))) {
    vwarning("View vector and up vector cannot be parallel when specifying a camera orientation.");
    return 0;
  }

  // Now we project the up vector onto the plane of the view vector
  up[0] -= mag*view[0];
  up[1] -= mag*view[1];
  up[2] -= mag*view[2];

  // and re-normalize it. We should not need to check for mag == 0
  // since we know the vectors were not parallel
  mag = sqrt(up[0]*up[0] + up[1]*up[1] + up[2]*up[2]);
  up[0] /= mag;
  up[1] /= mag;
  up[2] /= mag;

  //fprintf(stderr,"New Up <%f,%f,%f>\n",up[0],up[1],up[2]);

  // Finally we assign the new orientation
  mView[0] = view[0];
  mView[1] = view[1];
  mView[2] = view[2];

  mUp[0] = up[0];
  mUp[1] = up[1];
  mUp[2] = up[2];

  mRight[0] = mView[1]*mUp[2] - mView[2]*mUp[1];
  mRight[1] = mView[2]*mUp[0] - mView[0]*mUp[2];
  mRight[2] = mView[0]*mUp[1] - mView[1]*mUp[0];

  return 1;
}

int VisusCamera::setCamera(float pos[3], float view[3], float up[3])
{
  // First try to set the orientation
  if (orientation(view,up) == 0) {
    // If this failed we reject the change
    vwarning("Orientation invalid could not set camera.");
    return 0;
  }

  position(pos);

  return 1;
}

void VisusCamera::rotate(const VisusTransformation3D& accumulate, float* center, float x, float y)
{
  mModelView.rotate(accumulate,center,x,y);
}

void VisusCamera::translate(const VisusTransformation3D& accumulate, const VisusOpenGLState& state,
                            float x, float y)
{
  mModelView.translate(accumulate,state,x,y);
}

void VisusCamera::scale(float* center, float x, float y)
{
  mModelView.scale(center,x,y);
}

void VisusCamera::turn(float x, float y)
{
  float yaw; // The angle of rotation around the up vactor
  float pitch; // The angle of rotation aroun the right vector

  yaw = x*mRotationSpeed;
  pitch = y*mRotationSpeed;

  float yaw_quat[4];
  float pitch_quat[4];

  yaw_quat[0] = cos(yaw/2);
  yaw_quat[1] = sin(yaw/2)*mUp[0];
  yaw_quat[2] = sin(yaw/2)*mUp[1];
  yaw_quat[3] = sin(yaw/2)*mUp[2];
  
  pitch_quat[0] = cos(pitch/2);
  pitch_quat[1] = sin(pitch/2)*mRight[0];
  pitch_quat[2] = sin(pitch/2)*mRight[1];
  pitch_quat[3] = sin(pitch/2)*mRight[2];

  // combine the two rotations
  float rot[4];

  quaternionMultiply(yaw_quat,pitch_quat,rot);
  

  // Just to be on the save side we normalize the quaternion
  float mag;

  mag = sqrt(rot[0]*rot[0] + rot[1]*rot[1] + rot[2]*rot[2] + rot[3]*rot[3]);
  
  rot[0] /= mag;
  rot[1] /= mag;
  rot[2] /= mag;
  rot[3] /= mag;

  //fprintf(stderr,"yaw %f pitch %f    %f %f\n",yaw,pitch,x,y);
  //fprintf(stderr,"rotation: %f   <%f,%f,%f>\n",rot[0],rot[1],rot[2],rot[3]);


  float view[4];

  view[0] = 0;
  view[1] = mView[0];
  view[2] = mView[1];
  view[3] = mView[2];

  float up[4];
  
  up[0] = 0;
  up[1] = mUp[0];
  up[2] = mUp[1];
  up[3] = mUp[2];
  
  
  quaternionMultiply(rot,view,view);
  quaternionMultiply(rot,up,up);
  
  rot[1] *= -1;
  rot[2] *= -1;
  rot[3] *= -1;

  quaternionMultiply(view,rot,view);
  quaternionMultiply(up,rot,up);

  
  //fprintf(stderr,"View %f   <%f,%f,%f>\n",view[0],view[1],view[2],view[3]);
  //fprintf(stderr,"Up   %f   <%f,%f,%f>\n",up[0],up[1],up[2],up[3]);


  orientation(&(view[1]),&(up[1]));  
}

void VisusCamera::pan(float x, float y)
{
  float right = x*mPanSpeed;
  float up = y*mPanSpeed;

  mPosition[0] -= right*mRight[0] - up*mUp[0];
  mPosition[1] -= right*mRight[1] - up*mUp[1];
  mPosition[2] -= right*mRight[2] - up*mUp[2];

}

void VisusCamera::zoom(float x, float y)
{
  mViewAngle *= 1+y*sZoomFactorDefault;
}

void VisusCamera::setupOpenGL(VisusTransformation3D& model_view_3D)
{
  GLint viewport[4];

  // get the current viewport
  glGetIntegerv(GL_VIEWPORT,viewport);
  
  
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  
  
  switch (mCameraType) {
  case VISUS_ORTHOGRAPHIC: {
    // For an orthographic camera we use the extent of the near plane
    // as extent
    float height;
    float width;

    // First compute HALF the height of the near plane based on the angle
    height = -tan(mViewAngle/2)*(mNear+mFar)*0.5;
    //height = 10;

    // and then half its width following the aspect ratio
    width = height * viewport[2] / viewport[3];
    
    //fprintf(stderr,"Setting up orthograpic camera %f %f %f %f    %f %f\n",-width,width,-height,height,-mFar,mFar);
    glOrtho(-width,width,-height,height,mNear,mFar);
    break;
  }
  case VISUS_PERSPECTIVE:
    gluPerspective(mViewAngle,viewport[2] / (float)viewport[3],mNear,mFar);
    break;
  }

  gluLookAt(mPosition[0],mPosition[1],mPosition[2],
            mPosition[0]+mView[0],mPosition[1]+mView[1],mPosition[2]+mView[2],
            mUp[0],mUp[1],mUp[2]);
  

  model_view_3D *= mModelView;
  glMatrixMode(GL_MODELVIEW);  
  glPushMatrix();
  glLoadMatrixf(model_view_3D);
}

void VisusCamera::save(FILE* output)
{
  fprintf(output,"<VisusCamera>\n");

  fprintf(output,"%.8f %.8f %.8f %.8f\n",mModelView[0],mModelView[4],mModelView[8],mModelView[12]);
  fprintf(output,"%.8f %.8f %.8f %.8f\n",mModelView[1],mModelView[5],mModelView[9],mModelView[13]);
  fprintf(output,"%.8f %.8f %.8f %.8f\n",mModelView[2],mModelView[6],mModelView[10],mModelView[14]);
  fprintf(output,"%.8f %.8f %.8f %.8f\n",mModelView[3],mModelView[7],mModelView[11],mModelView[15]);

  fprintf(output,"CameraType %d\n",mCameraType);
  fprintf(output,"Position %.8f %.8f %.8f\n",mPosition[0],mPosition[1],mPosition[2]);
  fprintf(output,"View %.8f %.8f %.8f\n",mView[0],mView[1],mView[2]);
  fprintf(output,"Up %.8f %.8f %.8f\n",mUp[0],mUp[1],mUp[2]);
  fprintf(output,"Angle %.8f\n",mViewAngle);
  fprintf(output,"Near %.8f\n",mNear);
  fprintf(output,"Far %.8f\n",mFar);
  fprintf(output,"<VisusCamera>\n");
}

int VisusCamera::load(FILE* input)
{
  char token[100];

  fscanf(input,"%s",token);
  
  if (strcmp(token,"<VisusCamera>") != 0) {
    vwarning("Incorrect token cannot read in camera");
    return 0;
  }
  
  int tmp;

  fscanf(input,"%f %f %f %f",&(mModelView[0]),&(mModelView[4]),&(mModelView[8]),&(mModelView[12]));
  fscanf(input,"%f %f %f %f",&(mModelView[1]),&(mModelView[5]),&(mModelView[9]),&(mModelView[13]));
  fscanf(input,"%f %f %f %f",&(mModelView[2]),&(mModelView[6]),&(mModelView[10]),&(mModelView[14]));
  fscanf(input,"%f %f %f %f",&(mModelView[3]),&(mModelView[7]),&(mModelView[11]),&(mModelView[15]));

  fscanf(input,"%s %d",token,&tmp);
  mCameraType = (VisusCameraType)tmp;

  fscanf(input,"%s %f %f %f",token,&mPosition[0],&mPosition[1],&mPosition[2]);
  fscanf(input,"%s %f %f %f",token,&mView[0],&mView[1],&mView[2]);
  fscanf(input,"%s %f %f %f",token,&mUp[0],&mUp[1],&mUp[2]);
  fscanf(input,"%s %f",token,&mViewAngle);
  fscanf(input,"%s %f",token,&mNear);
  fscanf(input,"%s %f",token,&mFar);
  fscanf(input,"%s",token);

  if (strcmp(token,"<VisusCamera>") != 0) {
    fprintf(stderr,"token \"%s\"\n",token);
    vwarning("VisusCamera file input not consistent 2");
    return 0;
  }
  
  return 1;
}


void VisusCamera::quaternionMultiply(float u[4], float v[4], float w[4])
{
  float tmp[4];


  tmp[0] = u[0]*v[0] - (u[1]*v[1] + u[2]*v[2] + u[3]*v[3]);
  
  tmp[1] = u[2]*v[3] - u[3]*v[2] + u[0]*v[1] + v[0]*u[1];
  tmp[2] = u[3]*v[1] - u[1]*v[3] + u[0]*v[2] + v[0]*u[2];
  tmp[3] = u[1]*v[2] - u[2]*v[1] + u[0]*v[3] + v[0]*u[3];

  w[0] = tmp[0];
  w[1] = tmp[1];
  w[2] = tmp[2];
  w[3] = tmp[3];
}

VisusCamera interpolate(const VisusCamera& cam1, const VisusCamera& cam2, float t)
{
  VisusCamera half(cam1); // Copy *this camera

  // Make sure the camera we return is active
  half.active(true);

  // First we linearly interpolate all non-rotational attributes
  half.mPosition[0] = (1-t)*cam1.mPosition[0]  + t*cam2.mPosition[0];
  half.mPosition[1] = (1-t)*cam1.mPosition[1]  + t*cam2.mPosition[1];
  half.mPosition[2] = (1-t)*cam1.mPosition[2]  + t*cam2.mPosition[2];

  half.mViewAngle = (1-t)*cam1.mViewAngle + t*cam2.viewAngle();
  half.mNear = (1-t)*cam1.mNear + t*cam2.nearPlane();
  half.mFar = (1-t)*cam1.mFar + t*cam2.farPlane();
  
  // For consistency we also interpolated the
  // rotation/translation/panning speed

  half.mRotationSpeed = (1-t)*cam1.mRotationSpeed + t*cam2.mRotationSpeed;
  half.mPanSpeed = (1-t)*cam1.mPanSpeed + t*cam2.mPanSpeed;
  half.mZoomFactor = (1-t)*cam1.mZoomFactor + t*cam2.mZoomFactor;

  /*
  fprintf(stderr,"Position %.8f %.8f %.8f\n",half.mPosition[0],half.mPosition[1],half.mPosition[2]);
  fprintf(stderr,"View %.8f %.8f %.8f\n",half.mView[0],half.mView[1],half.mView[2]);
  fprintf(stderr,"Up %.8f %.8f %.8f\n",half.mUp[0],half.mUp[1],half.mUp[2]);
  fprintf(stderr,"Angle %.8f\n",half.mViewAngle);
  fprintf(stderr,"Near %.8f\n",half.mNear);
  fprintf(stderr,"Far %.8f\n",half.mFar);
  */
  // Now interpolate the camera rotation using quaternions
  VisusQuaternion q1,q2;
  VisusTransformation3D t1,t2;
  
  t1[0] = cam1.mView[0];  t1[4] = cam1.mUp[0]; t1[8] = cam1.mRight[0];
  t1[1] = cam1.mView[1];  t1[5] = cam1.mUp[1]; t1[9] = cam1.mRight[1];
  t1[2] = cam1.mView[2];  t1[6] = cam1.mUp[2]; t1[10] = cam1.mRight[2];

  t2[0] = cam2.mView[0];  t2[4] = cam2.mUp[0]; t2[8] = cam2.mRight[0];
  t2[1] = cam2.mView[1];  t2[5] = cam2.mUp[1]; t2[9] = cam2.mRight[1];
  t2[2] = cam2.mView[2];  t2[6] = cam2.mUp[2]; t2[10] = cam2.mRight[2];

  q1.fromMatrix(t1);
  q2.fromMatrix(t2);

  q1 = slerp(q1,q2,t);

  q1.toMatrix(t1);

  half.mView[0] = t1[0]; half.mUp[0] = t1[4]; half.mRight[0] = t1[8];
  half.mView[1] = t1[1]; half.mUp[1] = t1[5]; half.mRight[1] = t1[9];
  half.mView[2] = t1[2]; half.mUp[2] = t1[6]; half.mRight[2] = t1[10];

  

  // Finally we need to interpolate the modelview matrix
  half.mModelView = slerp(cam1.mModelView,cam2.mModelView,t);


  return half;
};

void VisusCamera::toXML(XMLNode& parent) const
{
  XMLNode cam = parent.addChild(XML_TAG);
  cam.addAttribute("active", mActive);
  cam.addAttribute("type", mCameraType);
  cam.addAttribute("viewAngle", mViewAngle);
  cam.addAttribute("near", mNear);
  cam.addAttribute("far", mFar);
  cam.addAttribute("rotationSpeed", mRotationSpeed);
  cam.addAttribute("panSpeed", mPanSpeed);
  cam.addAttribute("zoom", mZoomFactor);

  addChild(cam, "position", mPosition);
  addChild(cam, "view", mView);
  addChild(cam, "up", mUp);
  addChild(cam, "right", mRight);
  
  mModelView.toXML(cam);
}

bool VisusCamera::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusCamera did not receive top node");
    return false;
  }

  mActive = xmltobool(node.getAttribute("active"), true);
  mCameraType = (VisusCameraType)(xmltoi(node.getAttribute("type"), sCameraTypeDefault));
  mViewAngle = xmltof(node.getAttribute("viewAngle"), sViewAngleDefault);
  mNear = xmltof(node.getAttribute("near"), sNearPlaneDefault);
  mFar = xmltof(node.getAttribute("far"), sFarPlaneDefault);
  mRotationSpeed = xmltof(node.getAttribute("rotationSpeed"), sRotationSpeedDefault);
  mPanSpeed = xmltof(node.getAttribute("panSpeed"), sPanSpeedDefault);
  mZoomFactor = xmltof(node.getAttribute("zoom"), sZoomFactorDefault);

  getChild(node, "position", mPosition);
  getChild(node, "view", mView);
  getChild(node, "up", mUp);
  getChild(node, "right", mRight);
  
  XMLNode trans = node.getChildNode(VisusTransformation3D::XML_TAG);
  if (! mModelView.fromXML(trans))
    return false;

  return true;
}
