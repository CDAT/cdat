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


#ifndef VISUSGUIBASE_H
#define VISUSGUIBASE_H

#include <vector>

#include "VisusGroup.h"
#include "VisusOpenGLState.h"
#include "VisusTransformation3D.h"
#include "VisusBoundingBox.h"
#include "VisusLabelNode.h"

/*! Enum for all possible mouse buttons. Not that the first three are
 *  compatible with GLUT.
 */
enum VisusMouseButton {
  VISUS_LEFT_MOUSE   = 0,
  VISUS_MIDDLE_MOUSE  = 1,
  VISUS_RIGHT_MOUSE = 2,
  VISUS_THREE_MOUSE  = 3,
  VISUS_FOUR_MOUSE   = 4,
  VISUS_FIVE_MOUSE   = 5,
};

//! State of a mouse button, compatible with GLUT
enum VisusMouseState {
  VISUS_MOUSE_DOWN = 0,
  VISUS_MOUSE_UP   = 1,
};

enum VisusModifier {
  VISUS_SHIFT = 1 << 0,
  VISUS_CAPS  = 1 << 1,
  VISUS_CTRL  = 1 << 2,
  VISUS_ALT   = 1 << 3,
  VISUS_NUM   = 1 << 4,
  VISUS_META  = 1 << 5,

  VISUS_MODIFIER_MASK = (1 << 6)-1,
};
  

/*! This class implements a glut-like interface for a basic keyboard
 *  driven user-interface to the Visus library. However, the aim is to
 *  make the class independent of glut (or any other library) to be
 *  able to easily create multiple gui's using different
 *  libraries. There should *never* be any gui specific code in this
 *  class nor function to handle non-trivial gui interactions (e.g. no
 *  special functions for special menues of you Qt gui). Instead, this
 *  class is designed as a base class to more advanced gui classes and
 *  as such should remain as light weight as possible. To this end all
 *  functions are virtual to allow any child class to change the
 *  behavior as necessary. Finally, global and/or static variables
 *  should be used only very sparingly (or better not at all) as each
 *  instance of this class is supposed to represent a separate
 *  (independent) window.
 */
class VisusGuiBase
{
public:
  
  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  

  //! Default constructor
  VisusGuiBase(int width=VisusOpenGLState::sDefaultWidth, 
               int height=VisusOpenGLState::sDefaultHeight);

  virtual ~VisusGuiBase();

  /***************************************************************
   ******        Scenegraph interaction                  *********
   **************************************************************/  

  //! Return the root of the scenegraph
  virtual pVisusGroup getRoot() {return mRoot;}

  //! Set the root of the scenegraph
  /*! This function passes in a new scenegraph to the gui via the root
   *  of the graph. If the root is not a SceneNode its transformation
   *  will be adapted to fit into the standard world coordinate system
   *  based on its bounding box. A SceneNode is expected to contain an
   *  appropriate camera setup and not modified.
   */
  virtual int setRoot(pVisusGroup root);

  //! Set the focus noode 
  virtual int setFocus(pVisusGroup focus);
  /***************************************************************
   ******          GLUT emulation                        *********
   **************************************************************/  
  
  //! Function to setup the initial OpenGL state 
  virtual void initOpenGL();

  //! The display called use to draw the scene
  /*! Main display callback to draw the scene. Note that this function
   *  does *not* do a buffer swap. Each gui library has its own way of
   *  swapping buffers (not everyone calls glutSwapBuffer) nd
   *  therefore the actual swapping cannot be done here.
   */
  virtual void display();

  //! Function to indicate the display should be re-drawn
  /*! This function indicates that the display should be redrawn,
   *  e.g. glutPostRedisplay(). However, since each gui library
   *  implements this differently the baseclass only provides a hook
   */
  virtual void redisplay() = 0;

  //! Function to swap the buffers in dual buffering mode
  /*! This function swaps the buffers, e.g. glutSwapBuffers. To allow
   *  different gui libraries to implement their own version the
   *  baseclass only provides the hook.
   */
  virtual void swapBuffers() = 0;

  //! Mouse callback function
  /*! Callback function when a button is pressed or released.
   *  @param button: the button which has been pressed/released
   *  @param state: The current state of the button
   *  @param x: The x-coordinate in screen space at the time of the event
   *  @param y: The y-coordinate in screen space at the time of the event
   *            in OpenGL compatible coordinates (0 is the bottom of the 
   *            screen
   */
  virtual void mouse(int button, int state, int x, int y);

  
  //! Motion call back function
  /*! Callback function when the mouse is moved
   *  @param x: The x-coordinate in screen space at the time of the event
   *  @param y: The y-coordinate in screen space at the time of the event
   *            in OpenGL compatible coordinates (0 is the bottom of the 
   *            screen
   */
  virtual void motion(int x, int y);

  //! The keyboard callback function
  /*! Callback function when a non-modifier (no shift, alt etc.) key
   *  is pressed
   *  @param key: The ascii code of the key
   *  @param x: The x-coordinate in screen space at the time of the event
   *  @param y: The y-coordinate in screen space at the time of the event
   *            in OpenGL compatible coordinates (0 is the bottom of the 
   *            screen
   */
  virtual void keyboard(unsigned char key, int x, int y);

  //! Return the modifier of the last event
  /*! Callback to determine the current state of the modifier keys
   *  shuch as shift, ctrl etc.
   */
  virtual VisusModifier modifier() = 0;

protected:

  //! The width of the current window
  int mWindowWidth;

  //! The height of the current window
  int mWindowHeight;

  //! The x-coordinate of the last event
  int mPosX;

  //! The y-coordinate of the last event
  int mPosY;

  //! The current OpenGLState
  VisusOpenGLState mOpenGLState;
  
  //! Bitmask storing which mouse button is pressed
  unsigned char mMouseButton;

  //! The root of the current scene graph
  pVisusGroup mRoot;

  //! The current focus node
  pVisusGroup mFocus;

  //! Flag that stores wether the focus used to show the bbox
  bool mFocusBBoxFlag;

  //! The label to indicate the current focus
  pVisusLabelNode mFocusLabel;

  /***************************************************************
   ******          Protected Convinience Functions       *********
   **************************************************************/  
  
  //! Determine whether the given button is currently pressed
  bool isPressed(VisusMouseButton button);
};


#endif
