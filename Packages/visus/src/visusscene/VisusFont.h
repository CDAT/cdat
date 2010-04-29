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

#ifndef VISUSFONT_H
#define VISUSFONT_H

#ifdef WIN32
  #include <windows.h>
#endif

struct XMLNode;

#include <glew.h>


#ifdef VISUS_DISABLE_FTGL

  //! Dummy implementation of the FTFont API 
class VISUS_FTFont 
{
 public:
  void Depth(int d) {}
  void BBox(const char *st, float f0,float f1,float f2,float f3,
	    float f4,float f5) {}
  void FaceSize(int size) {}
  void Render(const char *st) {}
};

#else

//#define VISUS_FTGL_HEADER
#ifdef VISUS_FTGL_HEADER
  #include <FTGL/ftgl.h>
#else
  #include <FTGL/FTFont.h>
#endif
typedef FTFont VISUS_FTFont;
#endif

//! Font styles available via the FLGL library
/*! Enum of font styles available via the FTGL library. The enums are
 *  choosen to allow bit-matching of sets of font styles
 */
enum VISUS_FONT_STYLE {
  VISUS_BITMAP=0,
  VISUS_PIXMAP,
  VISUS_OUTLINE,
  VISUS_POLYGON,
  VISUS_EXTRUDE,
  VISUS_TEXTURE,
};

enum VisusFontScaling {
  VISUS_FONT_RELATIVE=0,
  VISUS_FONT_ABSOLUTE,
};

//! Wrapper around the FTGL font library
/*! This class provide an interface to the FTGL font rendering library
 *  and is designed as baseclass for all objects that need to render
 *  text. It provides access to all font atributes (e.g. size, font
 *  used, etc.) and rendering styles. The render method draws a piece
 *  of text at the origin of the current coordinate system.
 */
class VisusFont
{

 public:

  friend class VisusText;

  static const char* XML_TAG;

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/
  
  //! Default constructor
  VisusFont();

  //! Copy constructore
  VisusFont(const VisusFont& font);
  
  //! Destructor
  virtual ~VisusFont();

  VisusFont& operator=(const VisusFont& font); 
  
  /***************************************************************
   ******    Functionality to change font attributes     *********
   **************************************************************/
 
  //! Setting the font style
  /*! Virtual function to change the current font style. The function
   *  is virtual to allow derived classed to restrict their valid font
   *  types by overloading and cross-checking
   * @param[in] style: the new style
   */
  virtual void fontStyle(VISUS_FONT_STYLE style);

  //! Set the scaling type used for the font
  void fontScaling(VisusFontScaling scale) {mFontScaling = scale;}

  //! Setting the font size 
  /*! Function to change the font size
   * @param size[in]: the new font size
   * @param dpi[in]: the assumed "screen" resolution
   */
  void fontSize(int size, int dpi=72);
  
  //! Setting the font depth 
  /*! Function to change the font depth used in the VISUS_EXTRUDE style
   *  @param depth[in]: the new font depth
   */
  void fontDepth(float depth);

  //! Setting the true trype font used  
  /*! Function to change the font used during rendering 
   * @param file[in]: path to a .ttf file describing the new font
   */
  void fontFile(const char *file);
  
  //! Setting the font color
  /*! Alternative function to change the font color using uchars 
   * @param color[in]: the new color as 4 byte uchar array
   */    
  void fontColor(unsigned char color[4]);
  
  //! Setting the font color
  /*! Alternative function to change the font color using single
   *  uchars
   * @param r[in]: red component
   * @param g[in]: green component
   * @param b[in]: blue component
   * @param a[in]: alpha component   
   */    
  void fontColor(unsigned char r,unsigned char g, unsigned char b,
                 unsigned char alpha = 255);

  //! Function to call if any of the font geometry might have changed
  virtual void fontUpdate() {}
  
  /***************************************************************
   ******    Functionality to query font attributes      *********
   **************************************************************/

  //! Function to return the current font style
  VISUS_FONT_STYLE fontStyle() const {return mFontStyle;}

  //! Return the current font scaling
  VisusFontScaling fontScaling() const {return mFontScaling;}

  //! Function to return the current font size
  int fontSize() const {return mFontSize;} 

  //! Function to return the current font depth
  float fontDepth() const {return mFontDepth;}

  //! Function to return the current font color
  const unsigned char *fontColor() const {return mFontColor;}

  //! Function to return the path to the current .ttf font-file 
  const char *fontFile() const {return mFontFile;}

  /***************************************************************
   ******    Functionality to read / write XML          *********
   **************************************************************/

  //! Function to load XML data into instance
  bool fromXML(XMLNode& node);

  //! Function to build XML representing instance
  void toXML(XMLNode& parent) const;

  /***************************************************************
   ****** Virtual functions to set/query valid font styles  ******
   **************************************************************/
 
  //! Return the list of valid font styles
  /*! This function returns the set of fonts that can be rendered by
   *  setting the bit corresponding to a valid/invalid font to 1 and o
   *  respectively.
   * @return bit-encoded list of valid fonts 
   */
  virtual unsigned char validStyle() const {return ALL_STYLES_VALID;}

  //! Queries whether the given font style is valid
  /*! This function determines whether the given font style is valid
   *  for this class.
   * @param[in] style: the font style in question
   * @return true/false indicating whether this class can render in 
   *         the given style
   */
  virtual bool isValidStyle(VISUS_FONT_STYLE style) const;
  
  /***************************************************************
   ******   Protected functionality to interact with FTGL  *******
   **************************************************************/  

 protected:

  //! Return the bounding box of the given string
  /*! Function to return the bounding box of the given string in the
   *  local coordinate system
   * @param[in] st: string that should be rendered
   * @param[out] bb: bounding box stored as [min_x,min_y,min_z,max_x,
   *                 max_y,max_z]
   */
  void bbox(const char *st, float bb[6]);

  //! Return the current internal scaling factor
  float fontRenderScale() {return mFontRenderScale;}

  //! Scales the internal fontsize used for rendering
  /*! This function scales the internal font size which is used for
   *  rendering. The scale factor is used to adjust the font size to
   *  the canvas height 
   * @param scale: factor used for scaling
   */
  void fontRenderScale(float scale);

  //! Render the given string
  /*! Function to render the given string at the default position
   * which is the origin of the current coordinate system
   * @param[in] st: NULL terminated string that should be rendered
   */
  void render(const char *st);

  /***************************************************************
   ******       Private Functions                        *********
   **************************************************************/  
 private: 
  
  /***************************************************************
   ******        Internal Constants and Defaults         *********
   **************************************************************/
  
  //! Default rendering style
  static const VISUS_FONT_STYLE DEFAULT_FONT_STYLE;

  //! Default scaling type
  static const VisusFontScaling DEFAULT_FONT_SCALING;

  //! Constant used to indicate all possible styles are valid
  static const unsigned char ALL_STYLES_VALID;

  //! Default font size
  static const int DEFAULT_FONT_SIZE;

  //! Default fonst size multiplier
  /*! This constant is a convinience measure to make the font sizes
   *  more intuitive. Instead of setting huge font sizes we multiply
   *  the font size internally by this factor before rendering
   */
  static const int FONT_SIZE_MULTIPLIER;

  //! Default font depth used for VISUS_EXTRUDE style
  static const float DEFAULT_FONT_DEPTH;

  //! Default color
  static const unsigned char DEFAULT_FONT_COLOR[4];

  //! Default font file
  static const char *DEFAULT_FONT_FILE;


  //! Factory to create a FTGL font renderer using the current state
  /*! Internal function to create an FTGL font renderer using the
   * current state (font-file etc.).
   * @param[in] caller: string that describes the calling function
                        used for error messages in case of problems
     @return: pointer to a newly allocated FTGL font renderer
   */
  VISUS_FTFont *fontFactory(const char *caller);

 protected:

  /***************************************************************
   ******       Private Variables                        *********
   **************************************************************/  

  //! The current font style
  VISUS_FONT_STYLE mFontStyle;

  //! The current scaling type
  VisusFontScaling mFontScaling;

  //! The current font size
  int mFontSize;

  //! Scaling factor to adjust font sizes
  float mFontRenderScale;

  //! The depth of the extrusion
  float mFontDepth;

  //! Path to the currently used font file
  char mFontFile[1024];

  //! Font color
  unsigned char mFontColor[4];

  //! Pointer to the current FTGL font rendering engine
  VISUS_FTFont *mFontRenderer;
};

#endif
