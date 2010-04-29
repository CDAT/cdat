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

from pyvisus.core import VisusBoundingBox, VisusFont, VisusGroup, VisusColor, \
    VisusXMLInterface, VisusOpenGLState
from pyvisus.component import VisusSceneNode, VisusTickMarks, TM_Z_AXIS, TM_Y_AXIS

gWindow = None
gFocus = None
gRoot = None
winWidth = 800
winHeight= 600
gMouseX = 0
gMouseY = 0
gPressed = 0
gMouseMotion = False
xTickMarks = None
yTickMarks = None
zTickMarks = None

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


def display():
    global gWindow
    glutSetWindow(gWindow);

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
    global gWindow
    (x, y) = args
    glutSetWindow(gWindow);
    glViewport(0,0,x,y)
    glMatrixMode(GL_PROJECTION)
    glLoadIdentity()
    value = (10 * y) / (1.0 * x)
    glOrtho(-10,10,-1*value,value,-100,100)
    return


def keyboard( *args ):
    import sys
    (key, x, y) = args

    if key == 27:
      sys.exit(0)
    elif key == 'x':
      sys.exit(0)
    elif key == 'm':
      VisusXMLInterface().write("restart.xml", gRoot)
    return


def motion( *args ):
    global gFocus,gPressed,gMouseMotion,gMouseX,gMouseY
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
            gFocus.translate(newX,newY)
    elif gPressed == GLUT_RIGHT_BUTTON:
        gRoot.rotate(newX,newY)

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
    gWindow = glutCreateWindow("ViSUS Tick Marks Test")

    glutDisplayFunc( display )
    glutReshapeFunc( reshape )
    glutMouseFunc( mouse )
    glutKeyboardFunc( keyboard )
    glutMotionFunc( motion )
    glutIdleFunc( idle )

    #initDisplay()

    # Create the default scene graph 
    bbox = VisusBoundingBox()
    bbox.set(-5,-5,-5,5,5,5)
    #gRoot = VisusGroup.construct()
    gRoot = VisusSceneNode.construct()
    gRoot.setValue(bbox)
    gRoot.drawBoundingBox(True)
    gFocus = gRoot

    # Create Font 
    font = VisusFont()
    font.fontSize(5);

    # Create Tick Marks
    xTickMarks = VisusTickMarks.construct()
    axis = xTickMarks.axis()
    axis.legendText(" Test this X-Axis")
    axis.labelFont(font)
    xTickMarks.axis(axis)
    gRoot.attachSubTree(xTickMarks)

    # Create Tick Marks
    yTickMarks = VisusTickMarks.construct()
    yTickMarks.direction(TM_Y_AXIS)
    axis = yTickMarks.axis()
    axis.legendText("Y-Axis")
    axis.labelFont(font)
    yTickMarks.axis(axis)
    gRoot.attachSubTree(yTickMarks)

    # Create Tick Marks
    zTickMarks = VisusTickMarks.construct()
    zTickMarks.direction(TM_Z_AXIS)
    axis = zTickMarks.axis()
    axis.legendText("Z-Axis")
    axis.labelFont(font)
    axis.tickColor(VisusColor(1,0,0))
    zTickMarks.axis(axis)
    gRoot.attachSubTree(zTickMarks)

    # Run The Main
    glutMainLoop()


