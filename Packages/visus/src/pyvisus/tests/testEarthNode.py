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

from pyvisus.core import VisusBoundingBox, VisusEarthRadius, VisusFont, VisusXMLInterface, VisusOpenGLState
from pyvisus.component import VisusSceneNode
from pyvisus.data import VisusTexture
from pyvisus.display import VisusEarthNode

gBBox = None
window = None
gFocus = None
gRoot = None
winWidth = 800
winHeight= 600
gMouseX = 0
gMouseY = 0
gPressed = 0
gMouseMotion = False
gTime = 0

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
    glFlush ()
    glutSwapBuffers()
    return


def reshape( *args ):
    (x, y) = args
    glutSetWindow(window);
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
        gRoot.scale(newX,newY)

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
    glutSetWindow(window)
    gRoot.rotate(2.4/winWidth, 0.1/winHeight)
    glutPostRedisplay()
    return


if __name__ == "__main__":
	import sys
 
        filename = None

	newArgv = glutInit(sys.argv)
        for arg in newArgv:
           if arg.startswith("--file"):
              (opt,filename) = arg.split("=")

	glutInitDisplayMode( GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH | GLUT_MULTISAMPLE )
	glutInitWindowSize(winWidth, winHeight)
	glutInitWindowPosition(200, 200)
	window = glutCreateWindow("ViSUS Earth Test")

	glutDisplayFunc( display )
	glutReshapeFunc( reshape )
	glutMouseFunc( mouse )
	glutKeyboardFunc( keyboard )
	glutMotionFunc( motion )
	glutIdleFunc( idle )

        # Create the default scene graph 
        gBBox = VisusBoundingBox()
        gBBox.set(-5,-5,-5,5,5,5)
        gRoot = VisusSceneNode.construct()
        gRoot.setValue(gBBox)
	gFocus = gRoot

        # Create Earth Radius
	radius = VisusEarthRadius()
	radius.set(3.0)

        # Create Earth
        earth = VisusEarthNode.construct()
        earth.setValue(radius)
	gRoot.attachSubTree(earth)
        gFocus = earth

        # Load Texture If Given
        if filename is not None:
          texture = VisusTexture()
	  texture.loadPPM(filename)
	  earth.loadData(texture, 0)

        # Run The Main
	glutMainLoop()


