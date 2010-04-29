########################################################################
#
# Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
# Produced at the Lawrence Livermore National Laboratory  
# Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
# LLNL-CODE-406031.  
# All rights reserved.  
#   
# This file is part of "Simple and Flexible Scene Graph Version 2.0."
# Please also read BSD_ADDITIONAL.txt.
#   
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#   
# @ Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the disclaimer below.
# @ Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the disclaimer (as noted below) in
#   the documentation and/or other materials provided with the
#   distribution.
# @ Neither the name of the LLNS/LLNL nor the names of its contributors
#   may be used to endorse or promote products derived from this software
#   without specific prior written permission.
#   
#  
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
# LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING
#
########################################################################


"""Test simple functions (i.e. no pointers involved)"""
from OpenGL.GL import *
from OpenGL.GLU import *
from OpenGL.GLUT import *

from pyvisus.core import VisusFont, VisusColor, VisusXMLInterface, VisusOpenGLState
from pyvisus.component import * 
from pyvisus.core import banded, bgry, bry, gamma, \
    hot1, hot2, ice, lighthues, lut16, rich, smooth_rich


gColorMaps={"banded":banded(),
            "brgy":bgry(),
            "bry":bry(),
            "gamma":gamma(),
            "hot1":hot1(),
            "hot2":hot2(),
	        "ice":ice(),
            "lighthues":lighthues(),
            "lut16":lut16(),
	        "rich":rich(),
            "smooth_rich":smooth_rich()
	        }    

window = None
gFocus = None
gRoot = None
winWidth = 800
winHeight= 600
gMouseX = 0
gMouseY = 0
gPressed = 0
gMouseMotion = False
gColorMap = 0

def initDisplay():

    ambient  = [0, 0, 0, 1];
    diffuse  = [1, 1, 1, 1];
    specular = [1, 1, 1, 1];
    position = [1, 1, 1, 0];
    
    glLightfv(GL_LIGHT0, GL_AMBIENT,  ambient);
    glLightfv(GL_LIGHT0, GL_DIFFUSE,  diffuse);
    glLightfv(GL_LIGHT0, GL_SPECULAR, specular);
    glLightfv(GL_LIGHT0, GL_POSITION, position);

    glViewport(0, 0, winWidth, winHeight);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(-10, 10, -10*winHeight/float(winWidth), 10*winHeight/float(winWidth), -100, 100);

    glEnable(GL_DEPTH_TEST);
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);
    return


def display():
    glutSetWindow(window);

    state = VisusOpenGLState()
    state.fetchState()
    gRoot.setValue(state)

    glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    glMatrixMode(GL_MODELVIEW)
    glLoadIdentity()
    glColor3f(1,1,1)
    glLineWidth(1.0)
    gRoot.display()
    glutSwapBuffers()
    return


def reshape( *args ):
    (x, y) = args
    glutSetWindow(window);
    glViewport(0,0,x,y)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    glOrtho(-10,10,-10*3/4.0,10*3/4.0,-100,100)
    gluLookAt(0,0,10,0,0,-1,0,1,0)
    return


def keyboard( *args ):
    global gColorMap,gColorMaps

    import sys
    (key, x, y) = args

    if key == 27:
      sys.exit(0)
    elif key == 'x':
      sys.exit(0)
    elif key == 'm':
      VisusXMLInterface().write("restart.xml", gRoot)
    elif key == 'n':
      name = gColorMaps.keys()[gColorMap]
      map = gColorMaps[name]
      print "Change to color map... ", name
      gFocus.setValue(map)
      gColorMap += 1
      gColorMap %= len(gColorMaps)

    glutPostRedisplay()
    return


def motion( *args ):
    global gFocus,gPressed,gMouseMotion,gMouseX,gMouseY,gSceneFile
    (x, y) = args 

    if not gMouseMotion:
        return

    newX = (x-gMouseX) / (1.0 * winWidth)
    newY = (y-gMouseY) / (1.0 * winHeight)

    if gPressed == GLUT_LEFT_BUTTON:
        if gFocus is not None: 
            gFocus.rotate(newX,newY)

    elif gPressed == GLUT_MIDDLE_BUTTON:
        if gFocus is not None:
            gFocus.scale(newX,newY)

    elif gPressed == GLUT_RIGHT_BUTTON:
        if gFocus is not None:
            gFocus.translate(newX,newY)

    gMouseX = x
    gMouseY = y

    glutPostRedisplay()
    return


def mouse( *args ):
    global gPressed,gMouseMotion,gMouseX,gMouseY
    (button, state, x, y) = args

    if state == GLUT_DOWN:
        gPressed = button
        gMouseMotion = True
        gMouseX = x
        gMouseY = y
    else:
        gMouseMotion = False
        gPressed = -1

    glutPostRedisplay()
    return


def idle():
    return


if __name__ == "__main__":
  import sys

  newArgv = glutInit(sys.argv)

  glutInitDisplayMode( GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH | GLUT_MULTISAMPLE )
  glutInitWindowSize(winWidth, winHeight)
  glutInitWindowPosition(200, 200)
  window = glutCreateWindow("ViSUS Color Bar Test")

  glutDisplayFunc( display )
  glutReshapeFunc( reshape )
  glutMouseFunc( mouse )
  glutKeyboardFunc( keyboard )
  glutMotionFunc( motion )
  glutIdleFunc( idle )

  initDisplay()

  # Create the default scene graph 
  gRoot = VisusSceneNode.construct()

  # Create Font 
  font = VisusFont()
  font.fontSize(5);

  # Create First Color Bar 
  colorBar = VisusColorBar.construct()
  colorBar.setValue(VisusColor(1,0,0))
  axis = colorBar.axis()
  axis.legendText("Pressure")
  axis.labelFont(font)
  colorBar.axis(axis)
  gRoot.attachSubTree(colorBar)

  # Create Second Color Bar
  colorBar = VisusColorBar.construct()
  colorBar.position(-1,0)
  colorBar.orientation(BB_VERTICAL)
  axis = colorBar.axis()
  axis.tickPosition(AXIS_BOTH)
  axis.labelPosition(AXIS_BOTH)
  axis.legendText("Temperature")
  font.fontSize(3)
  axis.legendFont(font)
  font.fontSize(2)
  axis.labelFont(font)
  colorBar.axis(axis)
  gRoot.attachSubTree(colorBar)

  # Create Third Color Bar
  font.fontSize(3);
  colorBar = VisusColorBar.construct()
  colorBar.position(-1,-0.8)
  axis = colorBar.axis()
  axis.tickPosition(AXIS_HIGH)
  axis.legendAlignment(AXIS_LEFT_ALIGN)
  axis.legendPosition(AXIS_HIGH)
  axis.legendText("Density")
  axis.labelPosition(AXIS_HIGH)
  font.fontColor(0,0,255)
  axis.labelFont(font)
  colorBar.axis(axis)
  gRoot.attachSubTree(colorBar)
  gFocus = colorBar

  # Run The Main
  glutMainLoop()


