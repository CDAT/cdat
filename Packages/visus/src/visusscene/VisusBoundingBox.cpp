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


#include <cstdio>
#ifdef WIN32
#include <windows.h>
#endif
#include "glew.h"
#include "xmlParser.h"

#include "VisusBoundingBox.h"
#include "VisusAssert.h"

#include <algorithm>
#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif


const char* VisusBoundingBox::XML_TAG = "VisusBoundingBox";

const int VisusBoundingBox::LINE_SEGMENTS[3][4][2] = {
{
	// X-AXIS  Line Segments
	{ LEFT_BACK_LOWER_CORNER,  RIGHT_BACK_LOWER_CORNER }, 
	{ LEFT_FRONT_LOWER_CORNER, RIGHT_FRONT_LOWER_CORNER },
	{ LEFT_BACK_UPPER_CORNER, RIGHT_BACK_UPPER_CORNER },
	{ LEFT_FRONT_UPPER_CORNER, RIGHT_FRONT_UPPER_CORNER }
},
{
	// Y-AXIS  Line Segments
	{ LEFT_BACK_LOWER_CORNER, LEFT_BACK_UPPER_CORNER },
	{ LEFT_FRONT_LOWER_CORNER, LEFT_FRONT_UPPER_CORNER },
	{ RIGHT_BACK_LOWER_CORNER, RIGHT_BACK_UPPER_CORNER },
	{ RIGHT_FRONT_LOWER_CORNER, RIGHT_FRONT_UPPER_CORNER },
},
{
	// Z-AXIS  Line Segments
	{ LEFT_BACK_LOWER_CORNER, LEFT_FRONT_LOWER_CORNER },
	{ RIGHT_BACK_LOWER_CORNER, RIGHT_FRONT_LOWER_CORNER },
	{ LEFT_BACK_UPPER_CORNER, LEFT_FRONT_UPPER_CORNER },
	{ RIGHT_BACK_UPPER_CORNER, RIGHT_FRONT_UPPER_CORNER },
}
};

VisusBoundingBox::VisusBoundingBox()
{
  memset(mBBox,0,6*sizeof(float));
}

VisusBoundingBox::VisusBoundingBox(const VisusBoundingBox &bb)
{
  *this = bb;
}

VisusBoundingBox::VisusBoundingBox(float bb[6]) 
{
  memcpy(mBBox,bb,6*sizeof(float));
}

VisusBoundingBox::VisusBoundingBox(const std::vector<double>& left,
                                   const std::vector<double>& right)
{
  if ((left[0] > right[0]) || (left[1] > right[1]) || (left[2] > right[2])) {
    vwarning("Trying to set invalid Bounding Box ... ignored\n");
    memset(mBBox,0,6*sizeof(float));
    return;
  }

  mBBox[0] = left[0];
  mBBox[1] = left[1];
  mBBox[2] = left[2];
  mBBox[3] = right[0];
  mBBox[4] = right[1];
  mBBox[5] = right[2];
}

VisusBoundingBox::VisusBoundingBox(float lx, float ly, float lz, float hx, float hy, float hz)
{
  if ((lx > hx) || (ly > hy) || (lz > hz)) {
    vwarning("Trying to set invalid Bounding Box ... ignored\n");
    memset(mBBox,0,6*sizeof(float));
    return;
  }

  mBBox[0] = lx;
  mBBox[1] = ly;
  mBBox[2] = lz;
  mBBox[3] = hx;
  mBBox[4] = hy;
  mBBox[5] = hz;
}


VisusBoundingBox &VisusBoundingBox::operator=(const VisusBoundingBox &bb)
{
  memcpy(mBBox,bb.mBBox,6*sizeof(float));
  return *this;
}

VisusBoundingBox &VisusBoundingBox::operator+=(const VisusBoundingBox &bb)
{
  for (int i=0;i<3;i++) 
    mBBox[i] = std::min(mBBox[i],bb.mBBox[i]);

  for (int i=3;i<6;i++) 
    mBBox[i] = std::max(mBBox[i],bb.mBBox[i]);

  return *this;
}

bool VisusBoundingBox::operator==(const VisusBoundingBox& bb) const
{
  for (int i=0;i<6;i++) {
    if (mBBox[i] != bb.mBBox[i])
      return false;
  }

  return true;
}

VisusBoundingBox& VisusBoundingBox::scale(float s)
{
  float delta;

  for (int i=0;i<3;i++) {
    delta = mBBox[i+3] - mBBox[i];
    mBBox[i] -= (s-1)*delta*0.5;
    mBBox[i+3] += (s-1)*delta*0.5;
  }
}

std::vector<double> VisusBoundingBox::leftLower() const
{
  std::vector<double> corner(3);

  corner[0] = mBBox[0];
  corner[1] = mBBox[1];
  corner[2] = mBBox[2];
  
  return corner;
}

std::vector<double> VisusBoundingBox::rightUpper() const
{
  std::vector<double> corner(3);

  corner[0] = mBBox[3];
  corner[1] = mBBox[4];
  corner[2] = mBBox[5];
  
  return corner;
}

void VisusBoundingBox::center(float c[3]) const
{
  c[0] = (mBBox[0] + mBBox[3]) / 2;
  c[1] = (mBBox[1] + mBBox[4]) / 2;
  c[2] = (mBBox[2] + mBBox[5]) / 2;
}

void VisusBoundingBox::set(float minx, float miny, float minz, float maxx, 
                           float maxy, float maxz)
{
  if ((minx > maxx) || (miny > maxy) || (minz > maxz)) {
    vwarning("Trying to set invalid Bounding Box ... ignored\n");
    return;
  }

  mBBox[0] = minx;
  mBBox[1] = miny;
  mBBox[2] = minz;
  
  mBBox[3] = maxx;
  mBBox[4] = maxy;
  mBBox[5] = maxz;
  
}

void VisusBoundingBox::set(float bb[6])
{
  if ((bb[0] > bb[3]) || (bb[1] > bb[4]) || (bb[2] > bb[5])) {
    vwarning("Trying to set invalid Bounding Box ... ignored\n");
    return;
  }
  
  memcpy(mBBox,bb,6*sizeof(float));
}

void VisusBoundingBox::set(const std::vector<double>& left_lower, 
                           const std::vector<double>& right_upper)
{
  if ((left_lower[0] > right_upper[0]) || (left_lower[1] > right_upper[1]) 
      || (left_lower[2] > right_upper[2])) {
    vwarning("Trying to set invalid bounding box ... ignored.");
    return;
  }

  mBBox[0] = left_lower[0];
  mBBox[1] = left_lower[1];
  mBBox[2] = left_lower[2];

  mBBox[3] = right_upper[0];
  mBBox[4] = right_upper[1];
  mBBox[5] = right_upper[2];
}


bool VisusBoundingBox::valid() const
{
  if ((mBBox[0] <= mBBox[3]) && (mBBox[1] <= mBBox[4]) 
      && (mBBox[2] <= mBBox[5])) {
    return true; 
  }
  else
    return false;
}


VisusBoundingBox VisusBoundingBox::translate(float v[3]) const
{
  VisusBoundingBox bb;
  
  bb[0] = mBBox[0] + v[0];
  bb[3] = mBBox[3] + v[0];
  
  bb[1] = mBBox[1] + v[1];
  bb[4] = mBBox[4] + v[1];
  
  bb[2] = mBBox[2] + v[2];
  bb[5] = mBBox[5] + v[2];

  return bb;
}

VisusBoundingBox VisusBoundingBox::translate(float dx, float dy, float dz) const
{
  VisusBoundingBox bb;
  
  bb[0] = mBBox[0] + dx;
  bb[3] = mBBox[3] + dx;
  
  bb[1] = mBBox[1] + dy;
  bb[4] = mBBox[4] + dy;
  
  bb[2] = mBBox[2] + dz;
  bb[5] = mBBox[5] + dz;

  return bb;
}

VisusBoundingBox VisusBoundingBox::rotateRowMajor(const float m[16]) const
{
  VisusBoundingBox bb;
  VisusBoundingBox bounds;
  float v[3];
  int i;

  // Shift the current box to be centered around the origin
  bb = translate(-0.5*(mBBox[0]+mBBox[3]),-0.5*(mBBox[1]+mBBox[4]),
                 -0.5*(mBBox[2]+mBBox[5]));

  
  // transform the lower left corner and store in the new box
  rotateRowMajor(m,bb[0],bb[1],bb[2],v);
  
  bounds[0] = v[0]; bounds[1] = v[1];  bounds[2] = v[2]; 
  bounds[3] = v[0]; bounds[4] = v[1];  bounds[5] = v[2]; 
  

  rotateRowMajor(m,bb[0],bb[1],bb[5],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  rotateRowMajor(m,bb[0],bb[4],bb[2],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  rotateRowMajor(m,bb[0],bb[4],bb[5],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
    
  rotateRowMajor(m,bb[3],bb[1],bb[2],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  rotateRowMajor(m,bb[3],bb[1],bb[5],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  rotateRowMajor(m,bb[3],bb[4],bb[2],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  rotateRowMajor(m,bb[3],bb[4],bb[5],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  bb = bounds.translate(0.5*(mBBox[0]+mBBox[3]),0.5*(mBBox[1]+mBBox[4]),
                        0.5*(mBBox[2]+mBBox[5]));

  return bb;
}

VisusBoundingBox VisusBoundingBox::rotateColumnMajor(const float m[16]) const
{
  VisusBoundingBox bb;
  VisusBoundingBox bounds;
  float v[3];
  int i;

  // Shift the current box to be centered around the origin
  bb = translate(-0.5*(mBBox[0]+mBBox[3]),-0.5*(mBBox[1]+mBBox[4]),
                 -0.5*(mBBox[2]+mBBox[5]));

  
  // transform the lower left corner and store in the new box
  rotateColumnMajor(m,bb[0],bb[1],bb[2],v);
  
  bounds[0] = v[0]; bounds[1] = v[1];  bounds[2] = v[2]; 
  bounds[3] = v[0]; bounds[4] = v[1];  bounds[5] = v[2]; 
  

  rotateColumnMajor(m,bb[0],bb[1],bb[5],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  rotateColumnMajor(m,bb[0],bb[4],bb[2],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  rotateColumnMajor(m,bb[0],bb[4],bb[5],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
    
  rotateColumnMajor(m,bb[3],bb[1],bb[2],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  rotateColumnMajor(m,bb[3],bb[1],bb[5],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  rotateColumnMajor(m,bb[3],bb[4],bb[2],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  rotateColumnMajor(m,bb[3],bb[4],bb[5],v);
  for (i=0;i<3;i++)
    bounds[i] = std::min(bounds[i],v[i]);
  for (i=3;i<6;i++)
    bounds[i] = std::max(bounds[i],v[i-3]);
  
  bb = bounds.translate(0.5*(mBBox[0]+mBBox[3]),0.5*(mBBox[1]+mBBox[4]),
                        0.5*(mBBox[2]+mBBox[5]));

  return bb;
}

VisusTransformation3D VisusBoundingBox::translateToCenter() const
{
  return translationMatrix(-(mBBox[3]+mBBox[0])/2,
                           -(mBBox[4]+mBBox[1])/2,
                           -(mBBox[5]+mBBox[2])/2);
}

void VisusBoundingBox::display()
{
  glDisable(GL_LIGHTING);
  glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);

  glBegin(GL_QUADS);
  
  glVertex3f(mBBox[0],mBBox[1],mBBox[2]);
  glVertex3f(mBBox[3],mBBox[1],mBBox[2]);
  glVertex3f(mBBox[3],mBBox[4],mBBox[2]);
  glVertex3f(mBBox[0],mBBox[4],mBBox[2]);
  
  glVertex3f(mBBox[0],mBBox[1],mBBox[5]);
  glVertex3f(mBBox[3],mBBox[1],mBBox[5]);
  glVertex3f(mBBox[3],mBBox[4],mBBox[5]);
  glVertex3f(mBBox[0],mBBox[4],mBBox[5]);
  
  glEnd();

  glBegin(GL_LINES);

  glVertex3f(mBBox[0],mBBox[1],mBBox[2]);
  glVertex3f(mBBox[0],mBBox[1],mBBox[5]);
 
  glVertex3f(mBBox[3],mBBox[1],mBBox[2]);
  glVertex3f(mBBox[3],mBBox[1],mBBox[5]);
 
  glVertex3f(mBBox[3],mBBox[4],mBBox[2]);
  glVertex3f(mBBox[3],mBBox[4],mBBox[5]);
 
  glVertex3f(mBBox[0],mBBox[4],mBBox[2]);
  glVertex3f(mBBox[0],mBBox[4],mBBox[5]);
 
  /* 
  // These lines will draw the diagonals which can be useful for
  // debugging 

  glVertex3f(mBBox[0],mBBox[1],mBBox[2]);
  glVertex3f(mBBox[3],mBBox[4],mBBox[5]);
 
  glVertex3f(mBBox[3],mBBox[1],mBBox[2]);
  glVertex3f(mBBox[1],mBBox[4],mBBox[5]);
   
  glVertex3f(mBBox[0],mBBox[4],mBBox[2]);
  glVertex3f(mBBox[3],mBBox[1],mBBox[5]);

  glVertex3f(mBBox[0],mBBox[1],mBBox[5]);
  glVertex3f(mBBox[3],mBBox[4],mBBox[2]);
  */

  glEnd();
  glEnable(GL_LIGHTING);
}

void VisusBoundingBox::display2D()
{
  glDisable(GL_LIGHTING);

  glBegin(GL_QUADS);

  glVertex2f(mBBox[0],mBBox[1]);
  glVertex2f(mBBox[2],mBBox[1]);
  glVertex2f(mBBox[2],mBBox[3]);
  glVertex2f(mBBox[0],mBBox[3]);

  glEnd();

  glEnable(GL_LIGHTING);
}

void VisusBoundingBox::rotateRowMajor(const float m[16],float x, float y, float z,float res[3]) const
{
  res[0] = m[0]*x + m[1]*y + m[2]*z;
  res[1] = m[4]*x + m[5]*y + m[6]*z;
  res[2] = m[8]*x + m[9]*y + m[10]*z;
}

void VisusBoundingBox::rotateColumnMajor(const float m[16],float x, float y, float z,float res[3]) const
{
  res[0] = m[0]*x + m[4]*y + m[8]*z;
  res[1] = m[1]*x + m[5]*y + m[9]*z;
  res[2] = m[2]*x + m[6]*y + m[10]*z;
}


int VisusBoundingBox::getClosestToViewer(double points[NUM_CORNERS][3],
                                         double projPoints[NUM_CORNERS][3],
                                         int closest[NUM_CORNERS])
{
	double leftCorner[3]  = { mBBox[0], mBBox[1], mBBox[2] };
	double rightCorner[3] = { mBBox[3], mBBox[4], mBBox[5] };

	// Spell Out All Our Real Coordinates
	points[LEFT_BACK_LOWER_CORNER][0]=leftCorner[0]; 
	points[LEFT_BACK_LOWER_CORNER][1]=leftCorner[1];
	points[LEFT_BACK_LOWER_CORNER][2]=leftCorner[2];

	points[LEFT_BACK_UPPER_CORNER][0]=leftCorner[0];
	points[LEFT_BACK_UPPER_CORNER][1]=rightCorner[1];
	points[LEFT_BACK_UPPER_CORNER][2]=leftCorner[2];

	points[RIGHT_BACK_LOWER_CORNER][0]=rightCorner[0]; 
	points[RIGHT_BACK_LOWER_CORNER][1]=leftCorner[1];
	points[RIGHT_BACK_LOWER_CORNER][2]=leftCorner[2];

	points[RIGHT_BACK_UPPER_CORNER][0]=rightCorner[0];
	points[RIGHT_BACK_UPPER_CORNER][1]=rightCorner[1];
	points[RIGHT_BACK_UPPER_CORNER][2]=leftCorner[2];

  points[LEFT_FRONT_LOWER_CORNER][0]=leftCorner[0]; 
	points[LEFT_FRONT_LOWER_CORNER][1]=leftCorner[1];
	points[LEFT_FRONT_LOWER_CORNER][2]=rightCorner[2];

	points[LEFT_FRONT_UPPER_CORNER][0]=leftCorner[0];
	points[LEFT_FRONT_UPPER_CORNER][1]=rightCorner[1];
	points[LEFT_FRONT_UPPER_CORNER][2]=rightCorner[2];

	points[RIGHT_FRONT_LOWER_CORNER][0]=rightCorner[0]; 
	points[RIGHT_FRONT_LOWER_CORNER][1]=leftCorner[1];
	points[RIGHT_FRONT_LOWER_CORNER][2]=rightCorner[2];

	points[RIGHT_FRONT_UPPER_CORNER][0]=rightCorner[0];
	points[RIGHT_FRONT_UPPER_CORNER][1]=rightCorner[1];
	points[RIGHT_FRONT_UPPER_CORNER][2]=rightCorner[2];

	// Get OpenGL state
  GLdouble model[16];
	GLdouble proj[16]; 
	GLint    view[4];   
  GLdouble x, y, z;
	glGetDoublev(GL_MODELVIEW_MATRIX, model);
	glGetDoublev(GL_PROJECTION_MATRIX, proj);
	glGetIntegerv(GL_VIEWPORT, view);

	// Project Bounding Box Points To Viewer
	for (int i=0; i<NUM_CORNERS; ++i) {
			gluProject(points[i][0], points[i][1], points[i][2], model, proj, view, &x, &y, &z);	
      projPoints[i][0] = x;
      projPoints[i][1] = y;
      projPoints[i][2] = z;
	}
  
	// Determine Closest Point To Viewer (Z / depth will be smallest)
  sortPointsByDim(2, NUM_CORNERS, projPoints, closest);
  return closest[0];
}

void VisusBoundingBox::sortPointsByDim(const int dim, 
                                       const int numPoints, const double points[][3], 
                                       int closest[])
{
  closest[0] = 0;
  for (int i=1; i<numPoints; ++i) {
    bool saved = false;
    for (int j=0; j<i; ++j) {
      if (points[i][dim] < points[closest[j]][dim]) {
        // Shift Others
        for (int k=i; k>j; --k) {
          closest[k] = closest[k-1];
        }
        // Insert At J
	      closest[j]= i;        
        saved = true;
        break;
      }
    }
    if (!saved) {
      closest[i] = i;
    }
  }
}


void VisusBoundingBox::toXML(XMLNode& parent) const
{
  XMLNode box = parent.addChild(XML_TAG);
  std::stringstream ss;
  for (int i=0; i<6; ++i) {
    ss << mBBox[i] << " ";
  }
  box.addText(ss.str().c_str());
}


bool VisusBoundingBox::fromXML(XMLNode& node)
{
  if (strcmp(XML_TAG, node.getName())) 
  {
    std::stringstream ss;
    ss << "VisusBoundingBox did not received expected top level node. received(" << node.getName() << ")\n";
    vverbose(ss.str().c_str(), VISUS_XML_VERBOSE);
    vwarning(ss.str().c_str());
    return false;
  }

  std::stringstream ss;
  ss << node.getText();

  for (int i=0; i<6; ++i) {
    ss >> mBBox[i];
  }
  
  return true;
}
