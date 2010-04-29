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


#ifndef VISUSTEXT_H
#define VISUSTEXT_H

#include "VisusFont.h"
#include "VisusBoundingBox.h"
#include "VisusTransformation2D.h"

struct XMLNode;

//! Orientation of the text on the screen
/*! Enum of possible orientation in how text is rendered. If necessary
 *  this could be extended to include a freely rotating font (of
 *  course only for those fonts rendering styles that support this
 *  option)
 */
enum VISUS_TEXT_ORIENTATION {
  VISUS_LEFT_RIGHT=0,
  VISUS_RIGHT_LEFT,
  VISUS_TOP_BOTTOM,
  VISUS_BOTTOM_TOP,
};
  

//! OpenGL rendering of a single character array
/*! This class implements the OpenGL rendering of a single character
 *  array. The array is rendered as a single string as done by the FTGL
 *  library. In particular, there is no checking for line breaks which
 *  usually means that newlines are ignored or show up as strange
 *  symbols. The rendering is scaled to view a square screen as the
 *  [-1,1]^2 square and a potential rotation is stored
 *  locally.
 *
 *  Drawing the text without translation will draw the left lower
 *  corner of the text (which is almost but not quite its left lower
 *  bounding box corner) at the center of the screen. Drawing at
 *  position (-1,-1) will draw to the left lower corner of the screen
 *  etc.. Polygonal fonts can be rotated (around the center of their
 *  bounding box) by passing in a rotation matrix. For bitmapped fonts
 *  the rotation will be ignored.
 * 
 *  Due to the fact that bitmapped fonts cannot be scaled using
 *  glScale() scaling in this way should be avoided. If fonts are
 *  scaled nevertheless, bitmapped and polygonal fonts will render at
 *  different sizes (for the same font size) and potentially at
 *  different positions. To resize fonts one should therefore use the
 *  font size.
 */
class VisusText
{

 public:

  static const char* XML_TAG;
  
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/
  
  //! Default constructor
  VisusText(const char *st = NULL);
  
  //! Copy constructor
  VisusText(const VisusText& text);

  //! Destructor
  ~VisusText();

  /***************************************************************
   ******       Functions to set/get the text            *********
   **************************************************************/
  
  //! Function to set the text to be rendered
  void text(const char *st);

  //! Function to return a pointer to the text to be rendered
  const char *text() const;
    
  //! Return a copy of the local font used
  VisusFont font() const { return mFont; }

  //! Function to set the font for rendering
  void font(const VisusFont& font);

  //! Function to set the orientation of text
  void orientation(VISUS_TEXT_ORIENTATION orient);

  //! Return the orientation of the text
  VISUS_TEXT_ORIENTATION orientation() const { return mOrientation; }

  /***************************************************************
   ******       Functions to query text properties       *********
   **************************************************************/
  
  //! Bounding box scaled such that [-1,1] spans the canvas height
  /*! Return the bounding box of the current string in screen
   *  coordinates corresponding to a [-1,1]^2 screen. For polygonal
   *  the function return the bounding box of the rotated text.
   *  @param[in] st: Optional pointer to a string whose bounding
   *                 box we are interested in. If no string is 
   *                 given the currently stored mText is used.
   *  @return:the bounding box in the screen coordinate frame
   */
  VisusBoundingBox bbox(const char *st=NULL);

  //! Function to render the string 
  /*! Function to render the string into an OpenGL canvas of the given
   *  height. The font is scaled such that the canvasHeight
   *  corresponds to the interval [-1,1]. For example, calling
   *  render with no prior translation will put the left lower corner
   *  of the text at the center of the screen. (Note, that the left
   *  lower corner of the text is almost but not quite the left lower
   *  corner of its bounding box.) Translating by (-1,-1) will draw to
   *  the left lower corner of the canvas (assuming it is squared)
   *  etc.. IMPORTANT !!! One needs to be very careful when using
   *  glScale in conjunction with VisusText. Bitmapped fonts do NOT scale
   *  and the result will most likely be that different font will
   *  render at different sizes and potentially different places of
   *  the screen. To savely scale fonts modulate the font size.
   *  @param[in] screenHeight: the height of the current OpenGL window 
   *                          (used to scale the fonts)
   *  @param[in] st: Optional pointer to a string that should be
   *                 rendered. If no string is given the currently 
   *                 stored mText is used.
   */
  void render(int screenHeight, const char *st=NULL);

  //! Build instance data from XML data
  bool fromXML(XMLNode& node);

  //! Build XML data from instance data
  void toXML(XMLNode& node) const;
  
protected:
  
  //! The canvas height of the default screen
  static const float DEFAULT_CANVAS_HEIGHT;

  //! Pointer to a NULL terminated string containing the current 
  //!  text
  char *mText;
   
  //! Length of the current array
  int mBufferSize;

  //! Text Orientation
  VISUS_TEXT_ORIENTATION mOrientation;

  //! Local coordinate transform
  VisusTransformation2D mMatrix;

  //! Font used for rendering
  VisusFont mFont;

private:

  static const float sLeftRightMatrix[16];
  static const float sRightLeftMatrix[16];
  static const float sTopBottomMatrix[16];
  static const float sBottomTopMatrix[16];
};


#endif

