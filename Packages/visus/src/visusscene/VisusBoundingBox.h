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


#ifndef VISUSBOUNDINGBOX_H
#define VISUSBOUNDINGBOX_H
#ifdef WIN32
#include <windows.h>
#endif

#include <cstring>
#include <vector>

#include "VisusTransformation3D.h"

struct XMLNode;

enum VisusBoundingBoxIndex 
{
  LEFT_BACK_LOWER_CORNER=0, 
  LEFT_BACK_UPPER_CORNER, 
  RIGHT_BACK_LOWER_CORNER,  
  RIGHT_BACK_UPPER_CORNER,
  LEFT_FRONT_LOWER_CORNER, 
  LEFT_FRONT_UPPER_CORNER, 
  RIGHT_FRONT_LOWER_CORNER, 
  RIGHT_FRONT_UPPER_CORNER,
  NUM_CORNERS,
};


//! Three-dimensional bounding box
/*! This class implements a three-dimensional bounding box together
 *  with functionality to translate and rotate bounding boxes.
 */
class VisusBoundingBox
{
 public:
 
  static const char* XML_TAG;
  static const int LINE_SEGMENTS[3][4][2];

  VisusBoundingBox();
  VisusBoundingBox(const VisusBoundingBox &bb);
  VisusBoundingBox(float bb[6]);
  VisusBoundingBox(const std::vector<double>& left,
                   const std::vector<double>& right);
  VisusBoundingBox(float lx, float ly, float lz, float hx, float hy, float hz);

  virtual ~VisusBoundingBox() {}
  
  VisusBoundingBox &operator=(const VisusBoundingBox &bb);
  VisusBoundingBox &operator+=(const VisusBoundingBox &bb);
  float &operator[](int i) {return mBBox[i];}
  float operator[](int i) const {return mBBox[i];}
  bool operator==(const VisusBoundingBox& bb) const;
  bool operator!=(const VisusBoundingBox& bb) const {return !((*this) == bb);}
  
  VisusBoundingBox& scale(float s);

  void get(float bb[6]) const {memcpy(bb,mBBox,6*sizeof(float));}

  std::vector<double> leftLower() const;

  std::vector<double> rightUpper() const;

  void center(float c[3]) const;

  void set(float minx, float miny, float minz, float maxx, 
           float maxy, float maxz);

  void set(float bb[6]);

  void set(const std::vector<double>& left_lower, const std::vector<double>& right_upper);
  
  bool valid() const;

  //! Build instance from XML data, returns true on success
  bool fromXML(XMLNode& node);

  //! Build instance XML data into XML tree
  void toXML(XMLNode& parent) const;
 
  //! Return the translated box
  VisusBoundingBox translate(float v[3]) const;

   //! Return the translated box
  VisusBoundingBox translate(float dx, float dy, float dz) const;

 //! Return the rotated box 
  /*! Rotate the bounding around its center by the given matrix
   *  assumed to be in row-major order. This is NOT a full affine
   *  transformation only the upper 3-by-3 matrix is actually used.
   *  @param m: Transformation matrix in row-major order
   *  @return The bounding box rotated around its center
   */
  VisusBoundingBox rotateRowMajor(const float m[16]) const;
  
  //! Return the rotated box 
  /*! Rotate the bounding around its center by the given matrix
   *  assumed to be in column-major order. This is NOT a full affine
   *  transformation only the upper 3-by-3 matrix is actually used.
   *  @param m: Transformation matrix in column-major order
   *  @return The bounding box rotated around its center
   */  
  VisusBoundingBox rotateColumnMajor(const float m[16]) const;
  
  //! Add the negative translation to the center of the box to the
  //! given transformation
  VisusTransformation3D translateToCenter() const;

  //! Draw the bounding box in 3D
  void display();  

  //! Draw the bounding box in 2D
  void display2D();

  //! Returns the index of the point of bounding box closest to viewer and
  //  fills in the coordinates of the points, projected points, and the
  //  all ordered ascending list of sorted indices
  int getClosestToViewer(double points[NUM_CORNERS][3],
                         double projPoints[NUM_CORNERS][3],
                         int closest[NUM_CORNERS]);

  //! Sort the given list of points by the x(0), y(1), z(2) dimension and store ordered indices
  static void sortPointsByDim(const int dim, const int numPoints, const double points[][3], 
                              int closest[]);

protected:
  
  //! Bounds [min_x,min_y,min_z,max_x,max_y,max_z]
  float mBBox[6];

private:

  //! Rotate <x,y,z> by the given matrix and store the result in res
  void rotateRowMajor(const float m[16],float x, float y, float z,float res[3]) const;
    
  //! Rotate <x,y,z> by the given matrix and store the result in res
  void rotateColumnMajor(const float m[16],float x, float y, float z,float res[3]) const;

};

#endif
