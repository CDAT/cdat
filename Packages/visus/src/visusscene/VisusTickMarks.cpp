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

#include "xmlParser.h"

#include "VisusAssert.h"
#include "VisusTickMarks.h"
#include "VisusSharedBoundingBox.h"
#include "VisusSharedTransformation3D.h"

#include <math.h>
#include <GL/gl.h>

/***************************************************************
 ******       Public Default Values                    *********
 **************************************************************/ 

// All unites are in -1,1 coordinate system, with origin at 
// center of the GL drawing area.

const float         VisusTickMarks::DEFAULT_LENGTH            = 100;
const char          VisusTickMarks::DEFAULT_FORMAT_STRING[10] = "%0.1f";
const int           VisusTickMarks::DEFAULT_LABEL_SIZE        = 4;  
const float         VisusTickMarks::DEFAULT_LABEL_OFFSET      = 15.55;
const int           VisusTickMarks::DEFAULT_LEGEND_SIZE       = 6;
const AXISAlignment   VisusTickMarks::DEFAULT_LEGEND_ALIGNMENT  = AXIS_CENTER_ALIGN;
const float         VisusTickMarks::DEFAULT_LEGEND_OFFSET     = 10.53;
const float         VisusTickMarks::DEFAULT_MAJOR_LENGTH      = 0.05;
const float         VisusTickMarks::DEFAULT_MAJOR_WIDTH   = 0.2;
const float         VisusTickMarks::DEFAULT_MINOR_LENGTH      = 0.5;
const float         VisusTickMarks::DEFAULT_MINOR_WIDTH   = 0.2;
const float         VisusTickMarks::DEFAULT_MIN_VALUE         = 0;
const float         VisusTickMarks::DEFAULT_MAX_VALUE         = 1;
const int           VisusTickMarks::DEFAULT_MAJOR_TICKS       = 3;
const int           VisusTickMarks::DEFAULT_MINOR_TICKS       = 4;

/***************************************************************
 ******       Private Default Values                   *********
 **************************************************************/ 
const float VisusTickMarks::LEFT_RIGHT_MATRIX[16] = {1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1};
const float VisusTickMarks::BOTTOM_TOP_MATRIX[16] = {0,1,0,0,-1,0,0,0,0,0,1,0,0,0,0,1};
const float VisusTickMarks::TOP_BOTTOM_MATRIX[16] = {0,-1,0,0,1,0,0,0,0,0,1,0,0,0,0,1};  
const float VisusTickMarks::RIGHT_LEFT_MATRIX[16] = {-1,0,0,0,0,-1,0,0,0,0,1,0,0,0,0,1};


pVisusTickMarks VisusTickMarks::instantiate()
{
		return gObjectFactory.instantiate<VisusTickMarks>();
}


VisusTickMarks::VisusTickMarks():
    VisusGroup(VISUS_TICK_MARKS),
    mTMAxis(TM_X_AXIS),
    mLength(DEFAULT_LENGTH)
{
	VisusFont labelFont, legendFont;

	for (int j = 0; j < 3; ++j)
		mOrigin[j] = 0;
		
  mAxis.legendText("Legend");
  mAxis.legendAlignment(DEFAULT_LEGEND_ALIGNMENT);
  mAxis.legendOffset(DEFAULT_LEGEND_OFFSET);
  legendFont.fontSize(DEFAULT_LEGEND_SIZE);
  legendFont.fontStyle(VISUS_PIXMAP);
  mAxis.legendFont(legendFont);

  mAxis.labelText(DEFAULT_FORMAT_STRING);
  mAxis.labelOffset(DEFAULT_LABEL_OFFSET);
	labelFont.fontSize(DEFAULT_LABEL_SIZE);
  labelFont.fontStyle(VISUS_PIXMAP);
	mAxis.labelFont(labelFont);

  mAxis.majorTicks(DEFAULT_MAJOR_TICKS);
  mAxis.majorTickLength(DEFAULT_MAJOR_LENGTH);
  mAxis.majorTickThickness(DEFAULT_MAJOR_WIDTH);

  mAxis.minorTicks(DEFAULT_MINOR_TICKS);
  mAxis.minorTickLength(DEFAULT_MINOR_LENGTH);
  mAxis.minorTickThickness(DEFAULT_MINOR_WIDTH);

  mAxis.minValue(DEFAULT_MIN_VALUE);
  mAxis.maxValue(DEFAULT_MAX_VALUE);

  mDrawBoundingBox = false;
}

void VisusTickMarks::direction(const TMAxis axis)
{
	mTMAxis = axis;
}

float VisusTickMarks::getDimension(const TMAxis axis)
{
	switch(axis){
		case TM_X_AXIS:
			return fabs(mBBox[0] - mBBox[3]);
		case TM_Y_AXIS:
			return fabs(mBBox[1] - mBBox[4]);
		case TM_Z_AXIS:
			return fabs(mBBox[2] - mBBox[5]);
	}
	return 0;
}

const TMAxis VisusTickMarks::TICK_AXIS[3] = { 
	TM_Y_AXIS, // Ticks go in Y-AXIS when marking X-AXIS
	TM_X_AXIS, // Ticks go in X-AXIS when marking Y-AXIS
	TM_Y_AXIS  // Ticks go in Y-AXIS when marking Z-AXIS
};

void VisusTickMarks::getMostVisible(const VisusTransformation3D& modelView)
{
  // Set Our Total Distance Of Bar
  mLength = getDimension(mTMAxis);
  
  double points[NUM_CORNERS][3];
  double projPoints[NUM_CORNERS][3];
  int closestPoints[NUM_CORNERS];
  int closest = mBBox.getClosestToViewer(points, projPoints, closestPoints);

	// Clear Out Direction
	for (int i=0; i<3; ++i) {
		mDirection[i] = 0; 
		mTickDirection[i] = 0;
	}

	// Find Line Segment Which Matches
	for (int i=0; i<4; ++i) {
		for (int j=0; j<2; ++j) {
      if (closest == VisusBoundingBox::LINE_SEGMENTS[mTMAxis][i][j]) {
        int ndx = VisusBoundingBox::LINE_SEGMENTS[mTMAxis][i][0];

				// Set Origin Point Of Bar
				mOrigin[0]=points[ndx][0]; mOrigin[1]=points[ndx][1]; mOrigin[2]=points[ndx][2];

				// Set Direction Of Bar
				mDirection[mTMAxis] = 1;

				// Set Direction Of Ticks
				if (i<2) {
					mTickDirection[TICK_AXIS[mTMAxis]] = -1;
				}
				else {
					mTickDirection[TICK_AXIS[mTMAxis]] = 1;
				}
				break;
			}
		}
	}

}

void VisusTickMarks::printMatrix(const VisusTransformation3D& model_view_3D)
{
	printf("Matrix:\n");
	printf("%f %f %f %f\n", model_view_3D[0], model_view_3D[1], model_view_3D[2], model_view_3D[3]);
	printf("%f %f %f %f\n", model_view_3D[4], model_view_3D[5], model_view_3D[6], model_view_3D[7]);
	printf("%f %f %f %f\n", model_view_3D[8], model_view_3D[9], model_view_3D[10], model_view_3D[11]);
	printf("%f %f %f %f\n", model_view_3D[12], model_view_3D[13], model_view_3D[14], model_view_3D[15]);
}

void VisusTickMarks::displayBoundingBox() const
{
  VisusBoundingBox bbox;
  float dx,dy,dz;
  float longest;

  bbox = mBBox;

  dx = bbox[3] - bbox[0];
  dy = bbox[4] - bbox[1];
  dz = bbox[5] - bbox[2];

  longest = MAX(MAX(dx,dy),dz);
  
  dx = MAX(dx,0.05*longest);
  dy = MAX(dy,0.05*longest);
  dz = MAX(dz,0.05*longest);
  

  switch (mTMAxis) {
    
  case TM_X_AXIS:

    bbox[1] = mOrigin[1] - 0.1*dy;
    bbox[4] = mOrigin[1] + 0.1*dy;
    
    bbox[2] = mOrigin[2] - 0.1*dz;
    bbox[5] = mOrigin[2] + 0.1*dz;
    
    break;

  case TM_Y_AXIS:

    bbox[0] = mOrigin[0] - 0.1*dx;
    bbox[3] = mOrigin[0] + 0.1*dx;
    
    bbox[2] = mOrigin[2] - 0.1*dz;
    bbox[5] = mOrigin[2] + 0.1*dz;
    
    break;

  case TM_Z_AXIS:

    bbox[0] = mOrigin[0] - 0.1*dx;
    bbox[3] = mOrigin[0] + 0.1*dx;
    
    bbox[1] = mOrigin[1] - 0.1*dy;
    bbox[4] = mOrigin[1] + 0.1*dy;
    
    break;
  };
  
  
  //fprintf(stderr,"[%f,%f,%f] x [%f,%f,%f]\n",bbox[0],bbox[1],bbox[2],bbox[3],bbox[4],bbox[5]);

  mBoundingBoxColor.glColor();
  bbox.display();
    
}


void VisusTickMarks::display3D(VisusTransformation3D model_view_3D)
{
  VisusTransformation3D local;
  GLint viewport[4];  // x, y, width, height
  int canvas_width, canvas_height;

  getValue<VisusSharedTransformation3D>(local);
  if (mParent==NULL)
    getValue<VisusSharedBoundingBox>(mBBox);
  else
    mParent->getValue<VisusSharedBoundingBox>(mBBox);


  switch (mTMAxis) {
  case TM_X_AXIS:
    mAxis.minValue(mBBox[0]);
    mAxis.maxValue(mBBox[3]);
    break;
  case TM_Y_AXIS:
    mAxis.minValue(mBBox[1]);
    mAxis.maxValue(mBBox[4]);
    break;
  case TM_Z_AXIS:
    mAxis.minValue(mBBox[2]);
    mAxis.maxValue(mBBox[5]);
    break;
  }
    
  //fprintf(stderr,"Axis  [%f,%f] \n",mAxis.mMinValue,mAxis.mMaxValue);
  //fprintf(stderr,"VisusTickMarks bbox: [%f,%f,%f] x [%f,%f,%f]\n",mBBox[0],mBBox[1],mBBox[2],
  //        mBBox[3],mBBox[4],mBBox[5]);

  model_view_3D *= local;

  // Determine Where To Draw Ticks
  getMostVisible(model_view_3D);
 
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadMatrixf(model_view_3D);

  // Get the current viewport
  glGetIntegerv(GL_VIEWPORT,viewport);
  canvas_width = viewport[2];
  canvas_height = viewport[3];

  renderTickMarks(canvas_width, canvas_height);
       
  renderLegend(canvas_width,canvas_height);
        
  if (mDrawBoundingBox)
    displayBoundingBox();
 
  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display(model_view_3D);
        
  
  glPopMatrix();

	vglerror();
}


void
VisusTickMarks::renderTick(const float start[3], 
                           const float axisdir[3], // must be normalized
                           const float tickdir[3], // must be normalized
                           const float length,     // units of 'tickdir' vector
                           const float thickness   // as fraction of length
                           )
{
  float q[3];

  glDisable (GL_LIGHTING);

	//printf("renderTick() axisdir    = %8.4f %8.4f %8.4f\n", axisdir[0],axisdir[1],axisdir[2]);
  //printf("             tickdir    = %8.4f %8.4f %8.4f\n", tickdir[0],tickdir[1],tickdir[2]);
  //printf("             length     = %8.4f\n", length);
	//printf("             thickness  = %8.4f\n", thickness);
  glBegin(GL_QUADS);
  mAxis.mTickColor.glColor();

  // For some reason, the line below throws an invalid operation when running
  // through the python bindings
  //glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
  glVertex3fv(start);

  //fprintf(stderr, "  Tick: %8.4f %8.4f %8.4f\n",start[0],start[1],start[2]);

  for (int k = 0; k < 3; ++k) {
    q[k] = start[k] + tickdir[k] * length;
  }
  glVertex3fv(q);
  //fprintf(stderr, "        %8.4f %8.4f %8.4f\n",q[0],q[1],q[2]);

  for (int k = 0; k < 3; ++k) {
    q[k] += axisdir[k] * length * thickness;
  }
	glVertex3fv(q);
	//fprintf(stderr, "        %8.4f %8.4f %8.4f\n",q[0],q[1],q[2]);

	for (int k = 0; k < 3; ++k) {
		q[k] -= tickdir[k] * length;
	}
	glVertex3fv(q);
	//fprintf(stderr, "        %8.4f %8.4f %8.4f\n",q[0],q[1],q[2]);
	glEnd();

  glEnable (GL_LIGHTING);
  
  vglerror();
}


static double distFromBBoxCentroidToEdge(float vx, float vy,float x0, float y0,
                                         float x1, float y1)
{
	// Distance from centroid of bounding box to its edge
	// along vector (vx,vy);
	// This is used to compute the offsets needed to keep text
	// from interfering with tick marks.

	// Handle degenerate boxes
	if (x1 <= x0) return 0;
	if (y1 <= y0) return 0;

	// Symmetry allows us to consider only upper right
	// quadrant of bbox and rays from centroid.
	vx = fabs(vx);
	vy = fabs(vy);

	// Early return for horizontal and vertical lines
	const double eps = 1e-6;
	if (vx < eps) {
		return (y1-y0) / 2.0;
	}
	if (vy < eps) {
		return (x1-x0) / 2.0;
	}

	// centroid
	double xm = 0.5 * (x1 + x0);
	double ym = 0.5 * (y1 + y0);

	// assume parametric form:  p0 + u*v0 = p for each dimension
	// intersect with right edge first
	double dist = 0.0;
	double u = (x1 - xm) / vx;
	double y = ym + u * vy;
	if (y >= y0 && y <= y1) {
		dist = sqrt( (x1-xm)*(x1-xm) + (y-ym)*(y-ym) );
	} 
	else {
		// intersect with top edge
		u = (y1 - ym) / vy;
		double x = xm + u * vx;
		if (x >= x0 && x <= x1) {
			dist = sqrt( (x-xm)*(x-xm) + (y1-ym)*(y1-ym) );
		} 
		else {
			// Something went wrong, return half diagonal length
			dist = 0.5 * sqrt( (x1-x0)*(x1-x0) + (y1-y0)+(y1-y0) );
		}
	}
	return dist;
}

void
VisusTickMarks::renderLabel(const float   origin[3], 
                            const float   dir[3],	// normalized direction to offset from start
                            VisusText&    label,	// the label containing the text to render
                                                  // this should be const, but
                                                  // the screenBBox() method
                                                  // can not be const because it
                                                  // modifies the font scaling.
                            const char* text,
                            const float   offset,
                            const int     canvasWidth,
                            const int     canvasHeight)
{
	GLdouble dataPoints[3];
	for (int i=0; i<3; ++i)
		dataPoints[i] = origin[i] + (offset * mTickDirection[i]);

	glPushMatrix();

	glTranslatef(dataPoints[0], dataPoints[1], dataPoints[2]);
	label.render(canvasHeight,text);
	glTranslatef(-dataPoints[0], -dataPoints[1], -dataPoints[2]);

	glPopMatrix();

	// Current Visusnode processing requires final gl matrix mode
	// to be modelview.  This is just a guard against future
	// developers changing the matrix code above.

	glMatrixMode(GL_MODELVIEW);
	
	vglerror();
}


#define TICKLABELSLEN 512

void
VisusTickMarks::renderLabelForTick(const float start[3],
                                   const float tickdir[3],// must be normalized
                                   const float length,    // units of 'tickdir'
                                   VisusText&  label, // tick template
                                   const float tickVal,   // Value to display for the tick
                                   const int canvasWidth,
                                   const int canvasHeight)
{
	float q[3];
	char text[TICKLABELSLEN];

  if (mAxis.mDrawLabels == false) return;

	for (int k = 0; k < 3; ++k) {
		q[k] = start[k] + (tickdir[k] * length);
	}
	sprintf(text, label.text(), tickVal);
  
	//text[TICKLABELSLEN-1] = '\0';

	//printf("text = %s %f\n", mAxis.mLabel.text(),tickVal);

	renderLabel(q, tickdir, label, text,mAxis.mLabelOffset, canvasWidth, canvasHeight);
}


void VisusTickMarks::renderTickMarks(const int canvas_width, const int canvas_height)
{
	float value;  // value of major ticks
	// Spacing between ticks
	float delta = mLength / ((mAxis.mMajorTicks - 1)*(mAxis.mMinorTicks + 1));

	mAxis.mTickColor.glColor();

	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

	// We draw the lines as quads to enable proper off-screen high-res rendering
	// (You want the thickness of the lines to scale)

	float p[3];  // origin of current tick
	p[0] = mOrigin[0];
	p[1] = mOrigin[1];
	p[2] = mOrigin[2];

  //fprintf(stderr,"Axis  [%f,%f] \n",mAxis.mMinValue,mAxis.mMaxValue);

	float majorlen = mAxis.mMajorTickLength * mLength;
	for (int i = 0; i < mAxis.mMajorTicks-1; ++i) {

		renderTick(p, mDirection, mTickDirection, majorlen, mAxis.mMajorTickThickness);
   
		value = mAxis.mMinValue + i*(mAxis.mMaxValue - mAxis.mMinValue) / (mAxis.mMajorTicks - 1);

		renderLabelForTick(p, mTickDirection, majorlen, mAxis.mLabel, value, canvas_width, canvas_height);

		// Move to the start of the next tick
		for (int k = 0; k < 3; ++k) 
			p[k] += mDirection[k] * delta;

		for (int j = 0; j < mAxis.mMinorTicks; ++j) 
    {
			renderTick(p, mDirection, mTickDirection, mAxis.mMinorTickLength * majorlen, mAxis.mMinorTickThickness);

			// Move to the start of the next tick
			for (int k = 0; k < 3; ++k) 
				p[k] += mDirection[k] * delta;
		}
	}
	renderTick(p, mDirection, mTickDirection, majorlen, mAxis.mMajorTickThickness);

	value = mAxis.mMinValue + (mAxis.mMajorTicks-1)*(mAxis.mMaxValue - mAxis.mMinValue) 
				/ (mAxis.mMajorTicks - 1);

	renderLabelForTick(p, mTickDirection, majorlen, mAxis.mLabel, value, canvas_width, canvas_height);

	glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	
	vglerror();
}


void VisusTickMarks::renderLegend(const int canvasWidth, const int canvasHeight)
{
  char text[TICKLABELSLEN];
  float start[3];
  float q[3];  // the end of a major tick

  if (mAxis.mDrawLegend == false) return;

  for (int k = 0; k < 3; ++k) {
    float pos = 0.0;
    switch (mAxis.mLegendAlignment) 
    {
      case AXIS_LEFT_ALIGN: pos = 0; break;
      case AXIS_CENTER_ALIGN: pos = 0.5; break;
      case AXIS_RIGHT_ALIGN: pos = 1.0; break;
    }
    start[k] = mOrigin[k] + pos * mDirection[k] * mLength;
    q[k] = start[k] + (mTickDirection[k] * mAxis.mMajorTickLength * mLength) + (mTickDirection[k]*mAxis.mLabelOffset);
  }

  // Create a representative tick label to get the label bbox
  VisusBoundingBox bbox = mAxis.mLabel.bbox("1.0");

  double wbb = bbox[3] - bbox[0];  // width of bbox
  double hbb = bbox[4] - bbox[1];  // height of bbox
  double d =  sqrt(wbb*wbb + hbb*hbb);
  // Offset legend by 'd' so that it avoids ticks and tick labels

  //fprintf(stderr,"Offset length  %f\n",d);
  //fprintf(stderr,"Width  %d Height  %d\n",canvasWidth, canvasHeight);

  renderLabel(q, mTickDirection, mAxis.mLegend, mAxis.mLegend.text(),d + mAxis.mLegendOffset + mAxis.mLabelOffset, 
              canvasWidth, canvasHeight);

  // Compute starting point for legend positioning based on requested
  // alignment

  // Offset legend in the direction of ticks by its bounding box.
  vglerror();
}

void VisusTickMarks::toXMLLocalVariables(XMLNode& parent)
{
  parent.addAttribute("axis", mTMAxis);
  parent.addAttribute("length", mLength);

  mAxis.toXML(parent);
}


bool VisusTickMarks::fromXMLLocalVariables(XMLNode& node)
{
  if (! VisusGroup::fromXMLLocalVariables(node))
    return false;

  mTMAxis = (TMAxis) xmltoi(node.getAttribute("axis"), mTMAxis);
  mLength = xmltof(node.getAttribute("length"), mLength);

  XMLNode child = node.getChildNode(VisusBorderAxis::XML_TAG);
  if (! mAxis.fromXML(child)) {
    vwarning("Failed to retrieve border axis for VisusTickMarks");
    return false;
  }

  return true;
}
