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


#include <math.h>
#include <vector>
#include <algorithm>

#include "VisusTextureViewer.h"
#include "VisusSharedColorMap.h"
#include "VisusSharedDataRequest.h"

pVisusTextureViewer VisusTextureViewer::instantiate()
{
  return gObjectFactory.instantiate<VisusTextureViewer>();
}

VisusTextureViewer::VisusTextureViewer() : VisusGroup(VISUS_TEXTURE_VIEWER), VisusConsumer(1), 
  mOrientation(VISUS_ORIENTATION_XY), 
  mPIPSizeSet(false), mXSize(0), mYSize(0), 
  mPIPScaling(1.0)
{
  declareParameter<VisusSharedColorMap>();
  declareParameter<VisusSharedDataRequest>();
  declareParameter<VisusSharedTransformation2D>();

  mSinks[0] = &mData;
  mBorderColor[0]=1; mBorderColor[1]=0; mBorderColor[2]=0;
}

void VisusTextureViewer::pipPosition(const float x, const float y)
{
  VisusTransformation2D local;
  getValue(local);
  local[6] = x;   
  local[7] = y;   
  setValue(local);
}

void VisusTextureViewer::pipSize(const float xSize, const float ySize)
{
  mPIPSizeSet = true;
  mXSize = xSize;
  mYSize = ySize;
}

void VisusTextureViewer::pipSize(const float scaling)
{
  mPIPSizeSet = false;
  mPIPScaling = scaling;
}

void VisusTextureViewer::pipColor(const float color[3])
{
  for (int i=0; i<3; ++i)
    mBorderColor[i] = color[i];
}

void VisusTextureViewer::pipColor(const float red, const float green, const float blue)
{
  mBorderColor[0] = red;
  mBorderColor[1] = green;
  mBorderColor[2] = blue;
}

void VisusTextureViewer::orientation(VisusSliceOrientation o)
{
  VisusTransformation3D matrix;
  VisusDataRequest request;

  mOrientation = o;

  switch (mOrientation) {
    
  case VISUS_ORIENTATION_XY:
    matrix = xyPlane();
    break;
  case VISUS_ORIENTATION_XZ:
    matrix = xzPlane();
    break;
  case VISUS_ORIENTATION_YZ:
    matrix = yzPlane();
    break;
  }

  
  // Load the current request
  getValue(request);

  request.transformation().setRotation(matrix);

  // Share the new request
  setValue(request);

  // If we have a valid producer
  if (this->mInputNodes[0] != NULL)
    this->mInputNodes[0]->interrupt(); // Indicate that a new request has been posted   
}


void VisusTextureViewer::translateRequest(float x, float y)
{
  VisusDataRequest request;
  float move;

  // Depending on the orientation of the slice on the screen the user
  // is likely to translate a slice either by moving "only" in x or
  // only in y direction. Rather than using only x or only y we try to
  // accommodate this by moving according to the larger value
  if (fabs(x) >= fabs(y))
    move = x;
  else
    move = -y;

  // Load the current request
  getValue(request);

  move *= request.transformation().translationSpeed();

  switch (mOrientation) {
    
  case VISUS_ORIENTATION_XY:    
    request.transformation()[14] += move;
    break;
  case VISUS_ORIENTATION_XZ:    
    request.transformation()[13] += move;
    break;
  case VISUS_ORIENTATION_YZ:    
    request.transformation()[12] += move;
    break;
  }

  // Publish the modified request
  setValue(request);
  
  // If we have a valid producer
  if (this->mInputNodes[0] != NULL)
    this->mInputNodes[0]->interrupt(); // Indicate that a new request has been posted   
}
    
    
int VisusTextureViewer::connectInput(pVisusProducer producer)
{
  if (connect(0,producer,&mData) == 0) {
    vwarning("Could not connect producer.");
    return 0;
  }

  return 1;
}

void VisusTextureViewer::displayBoundingBox() const
{
  VisusBoundingBox bbox;
  getValue(bbox);
  bbox.display();
}

void VisusTextureViewer::display(VisusTransformation3D model_view_3D,
                                 VisusTransformation2D model_view_2D,
                                 bool lock_graph)
{
  VisusDataRequest request;
  getValue(request);

  synchronize();

  setBBoxScreenCoords();

  // Display Our Bounding Box
  if (mDrawBoundingBox)
    displayBoundingBox();

  enter2DRenderMode();
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();

  renderPIP(model_view_3D, model_view_2D);
  renderFrame();
  renderLines(model_view_3D, model_view_2D);

  glPopMatrix();
  exit2DRenderMode();
}


void VisusTextureViewer::setBBoxScreenCoords()
{
  VisusBoundingBox bbox;
  getValue(bbox);

  double points[NUM_CORNERS][3];
  int closestPoints[NUM_CORNERS];
  bbox.getClosestToViewer(points, mProjBoxPoints, closestPoints);

  // The Four Closest Points Best Represent Front Face
  double frontPoints[4][3];
  int leftMost[4];
  for (int i=0; i<4; ++i) {
    int ndx = closestPoints[i];
    for (int j=0; j<3; ++j)
      frontPoints[i][j] = mProjBoxPoints[ndx][j];
  }

  // Sort Front Face By Ascending X
  VisusBoundingBox::sortPointsByDim(0, 4, frontPoints, leftMost);

  // Assign Lefters
  int ndx1=leftMost[0]; 
  int ndx2=leftMost[1];
  if (frontPoints[ndx1][1] < frontPoints[ndx2][1]) {
    int tmp = ndx2;
    ndx2 = ndx1;
    ndx1 = tmp;
  }
  mLeftUpper[0]=frontPoints[ndx1][0]; mLeftUpper[1]=frontPoints[ndx1][1]; mLeftUpper[2]=frontPoints[ndx1][2];
  mLeftLower[0]=frontPoints[ndx2][0]; mLeftLower[1]=frontPoints[ndx2][1]; mLeftLower[2]=frontPoints[ndx2][2];

  // Assign Righters
  ndx1=leftMost[2]; 
  ndx2=leftMost[3];
  if (frontPoints[ndx1][1] < frontPoints[ndx2][1]) {
    int tmp = ndx2;
    ndx2 = ndx1;
    ndx1 = tmp;
  }
  mRightUpper[0]=frontPoints[ndx1][0]; mRightUpper[1]=frontPoints[ndx1][1]; mRightUpper[2]=frontPoints[ndx1][2];
  mRightLower[0]=frontPoints[ndx2][0]; mRightLower[1]=frontPoints[ndx2][1]; mRightLower[2]=frontPoints[ndx2][2];

}


void VisusTextureViewer::renderPIP(VisusTransformation3D& model_view_3D,
                                   VisusTransformation2D& model_view_2D)
 {
  VisusTransformation2D local2D;
  pVisusSharedColorMap color_map;
  std::vector<double> extent(3,0);
  float tex_max[2];

  getValue(local2D);  
  color_map = sharedValue<VisusSharedColorMap>();

  if (color_map->lockToRead() == 0) {
    vwarning("Could not lock color map for reading. Rendering aborted.");
    return;
  }

  // Move Texture To Center Of Extent
  VisusTransformation3D center3D = mData.translateToCenter();
  VisusTransformation2D center2D;
  center2D[6] = center3D[12];
  center2D[7] = center3D[13];
  model_view_2D *= center2D;

  // Translate To PIP Position
  model_view_2D *= local2D;

  glTranslatef(model_view_2D[6], model_view_2D[7], 0);
  glColor3f(1, 1, 1);  // reset color

  if (mData.preDraw(*color_map) != 0) {
    
    setExtent();

    mData.texMax(tex_max,tex_max+1);

    extent = mData.extent();

    glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

    glBegin(GL_QUADS);
    {
      glTexCoord2f(0,0);                  glVertex3f(0,0,0);
      glTexCoord2f(tex_max[0],0);         glVertex3f(extent[0],0,0);
      glTexCoord2f(tex_max[0],tex_max[1]);glVertex3f(extent[0],extent[1],0);
      glTexCoord2f(0,tex_max[1]);         glVertex3f(0,extent[1],0);
    }
    glEnd();
  }  
  mData.postDraw();
  
  if (color_map->unlockAfterRead() == 0) {
    vwarning("Could not unlock color map after renderin. Aborting rendring.");
    return;
  }

  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glDisable(GL_LIGHTING);
}


void VisusTextureViewer::setExtent()
{
  VisusBoundingBox bbox;
  std::vector<double> values;
  std::vector<double> extent(3,0);
  float tex_max[2];
  GLint viewport[4];
	GLdouble modelview[16];
	GLdouble projection[16];

  getValue(bbox);
	glGetDoublev( GL_MODELVIEW_MATRIX, modelview );
	glGetDoublev( GL_PROJECTION_MATRIX, projection );
	glGetIntegerv( GL_VIEWPORT, viewport );

  mData.texMax(tex_max,tex_max+1);

  if (mPIPSizeSet) 
  {
    // Extent Is PIP Size
    values.push_back(mXSize);  // x
    values.push_back(mYSize);  // y
  }
  else
  {
    // Extent is Scaling of Producer BBox With Image Aspect Ratio
    std::vector<double> left, right;
  
    left = bbox.leftLower();
    right= bbox.rightUpper();
    
    double x = right[0] - left[0];
    double y = right[1] - left[1];

    double dim3_to_dim2_factor = 0.1;  // let's call this the fudge factor

    float h_to_w = tex_max[1] / tex_max[0];

    double newX = x * dim3_to_dim2_factor * mPIPScaling;
    double newY = y * dim3_to_dim2_factor * h_to_w * mPIPScaling;

    values.push_back(newX);   // x
    values.push_back(newY);   // y
  }
  values.push_back(0); // z

  // Set Extent
  mData.extent(values);
}

void VisusTextureViewer::renderFrame()
{
  std::vector<double> extent(3,0);

  extent = mData.extent();
  glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
  glColor3f(mBorderColor[0],mBorderColor[1],mBorderColor[2]);
  glBegin(GL_QUADS);
  {
    glVertex3f(0,0,0);
    glVertex3f(extent[0],0,0);
    glVertex3f(extent[0],extent[1],0);
    glVertex3f(0,extent[1],0);
  }
  glEnd();
}

void VisusTextureViewer::renderLines(VisusTransformation3D& model_view_3D,
                                     VisusTransformation2D& model_view_2D)
{
  // Get Texture Box
  std::vector<double> extent(3,0);
  extent = mData.extent();

  GLint viewport[4];
  GLdouble modelview[16];
  GLdouble projection[16];
  GLdouble posX, posY, posZ;
  
  glGetDoublev( GL_MODELVIEW_MATRIX, modelview );
  glGetDoublev( GL_PROJECTION_MATRIX, projection );
  glGetIntegerv( GL_VIEWPORT, viewport );

  // Draw The Two Lines
  double linePoint[2][2][2];

  glBegin(GL_LINES);
  if (model_view_2D[6] < 0) {
 
    // Draw Right Side PIP to Left Side BBox
    gluUnProject( mLeftUpper[0], mLeftUpper[1], mLeftUpper[2], modelview, projection, viewport, &posX, &posY, &posZ);
    linePoint[0][0][0]=posX;      linePoint[0][0][1]=posY;
    linePoint[0][1][0]=extent[0]; linePoint[0][1][1]=extent[1];

 	  gluUnProject( mLeftLower[0], mLeftLower[1], mLeftLower[2], modelview, projection, viewport, &posX, &posY, &posZ);
    linePoint[1][0][0]=posX;      linePoint[1][0][1]=posY;
    linePoint[1][1][0]=extent[0]; linePoint[1][1][1]=0;
  }
  else {
    // Draw Left Side PIP to Right Side BBox
    gluUnProject( mRightUpper[0], mRightUpper[1], mRightUpper[2], modelview, projection, viewport, &posX, &posY, &posZ);
    linePoint[0][0][0]=posX;   linePoint[0][0][1]=posY;
    linePoint[0][1][0]=0;      linePoint[0][1][1]=extent[1];

 	  gluUnProject( mRightLower[0], mRightLower[1], mRightLower[2], modelview, projection, viewport, &posX, &posY, &posZ);
    linePoint[1][0][0]=posX;   linePoint[1][0][1]=posY;
    linePoint[1][1][0]=0;      linePoint[1][1][1]=0;
  }

  // If Lines Intersect, Swap BBox End-Points
  if (linesIntersect(linePoint[0][0], linePoint[0][1], linePoint[1][0], linePoint[1][1], posX, posY))
  {
    double tmp[2];
    tmp[0] = linePoint[0][0][0]; tmp[1] = linePoint[0][0][1];
    linePoint[0][0][0] = linePoint[1][0][0]; linePoint[0][0][1] = linePoint[1][0][1];
    linePoint[1][0][0] = tmp[0];   linePoint[1][0][1] = tmp[1];
  }

  // Draw Lines
  for (int i=0; i<2; ++i) {
    for (int j=0; j<2; ++j) {
      glVertex2f(linePoint[i][j][0], linePoint[i][j][1]);
    }
  }

  glEnd();
}


#define SAME_SIGNS( a, b )	\
		(((long) ((unsigned long) a ^ (unsigned long) b)) >= 0 )

bool VisusTextureViewer::linesIntersect(const double line1P1[], const double line1P2[], /* First line segment */
                                       const double line2P1[], const double line2P2[], /* First line segment */
		                                   double& x, double& y /* Output Points of Intersection */ )
{
  double x1,y1,x2,y2,x3,y3,x4,y4;
  x1=line1P1[0]; y1=line1P1[1];
  x2=line1P2[0]; y2=line1P2[1];
  x3=line2P1[0]; y3=line2P1[1];
  x4=line2P2[0]; y4=line2P2[1];

  double a1, a2, b1, b2, c1, c2; /* Coefficients of line eqns. */
  long r1, r2, r3, r4;         /* 'Sign' values */
  double denom, num;     /* Intermediate values */

  /* Compute a1, b1, c1, where line joining points 1 and 2
   * is "a1 x  +  b1 y  +  c1  =  0".
   */
  a1 = y2 - y1;
  b1 = x1 - x2;
  c1 = x2 * y1 - x1 * y2;

  /* Compute r3 and r4.
   */
  r3 = a1 * x3 + b1 * y3 + c1;
  r4 = a1 * x4 + b1 * y4 + c1;

  /* Check signs of r3 and r4.  If both point 3 and point 4 lie on
   * same side of line 1, the line segments do not intersect.
   */
  if ( r3 != 0 && r4 != 0 && SAME_SIGNS( r3, r4 ))
    return false;  // don't intersect

  /* Compute a2, b2, c2 */
  a2 = y4 - y3;
  b2 = x3 - x4;
  c2 = x4 * y3 - x3 * y4;

  /* Compute r1 and r2 */
  r1 = a2 * x1 + b2 * y1 + c2;
  r2 = a2 * x2 + b2 * y2 + c2;

  /* Check signs of r1 and r2.  If both point 1 and point 2 lie
   * on same side of second line segment, the line segments do
   * not intersect.
   */
  if ( r1 != 0 && r2 != 0 && SAME_SIGNS( r1, r2 ))
    return false;  // don't intersect

  /* Line segments intersect: compute intersection point. 
   */

  denom = a1 * b2 - a2 * b1;
  if ( denom == 0 )
    return false ; // co-linear

  num = b1 * c2 - b2 * c1;
  x = num / denom;

  num = a2 * c1 - a1 * c2;
  y = num / denom;

  // Okay, Now Need To Ensure Within Line Segments
  double minx = std::min(x1,x2);
  double maxx = std::max(x1,x2);
  double miny = std::min(y1,y2);
  double maxy = std::max(y1,y2);

  if (x > maxx || x < minx || y > maxy || y < miny) {
    return false;
  }

  minx = std::min(x3,x4);
  maxx = std::max(x3,x4);
  miny = std::min(y3,y4);
  maxy = std::max(y3,y4);

  if (x > maxx || x < minx || y > maxy || y < miny) {
    return false;
  }

  return true;  // intersection
} /* lines_intersect */


void VisusTextureViewer::toXMLLocalVariables(XMLNode& node)
{
  node.addAttribute("orientation", mOrientation);

  mData.toXML(node);

  XMLNode child = node.addChild("borderColor");
  child.addAttribute("r", mBorderColor[0]);
  child.addAttribute("g", mBorderColor[1]);
  child.addAttribute("b", mBorderColor[2]);

  node.addAttribute("pipSizeSet", mPIPSizeSet);
  if (mPIPSizeSet)
  {
    node.addAttribute("pipXSize", mXSize);
    node.addAttribute("pipYSize", mYSize);
  }
  node.addAttribute("pipScale", mPIPScaling);
}

bool VisusTextureViewer::fromXMLLocalVariables(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) {
    vwarning("VisusTextureViewer did not receive top level node");
    return false;
  } 
  mOrientation = (VisusSliceOrientation) xmltoi(node.getAttribute("orientation"), mOrientation);

  XMLNode child = node.getChildNode(VisusData::XML_TAG);
  if (! mData.fromXML(child)) {
    vwarning("Failed to load texture for VisusTextureViewer");
    return false;
  }

  child = node.getChildNode("borderColor");
  mBorderColor[0] = xmltof(child.getAttribute("r"), mBorderColor[0]);
  mBorderColor[1] = xmltof(child.getAttribute("g"), mBorderColor[1]);
  mBorderColor[2] = xmltof(child.getAttribute("b"), mBorderColor[2]);

  mPIPSizeSet = xmltobool(node.getAttribute("pipSizeSet"), mPIPSizeSet);
  if (mPIPSizeSet)
  {
    mXSize = xmltof(node.getAttribute("pipXSize"), mXSize);
    mYSize = xmltof(node.getAttribute("pipYSize"), mYSize);
  }
  mPIPScaling = xmltof(node.getAttribute("pipScale"), mPIPScaling);
  
  return true;
}
