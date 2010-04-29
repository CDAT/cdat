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

from pyvisus.core import VisusFont, VISUS_ORIENTATION_YZ, VectorInt, VectorDouble, \
    translationMatrix, VisusBoundingBox, VisusGroup, VisusXMLInterface, VisusOpenGLState
from pyvisus.component import VisusSphereNode, VisusSceneNode, VisusLabelNode
from pyvisus.data import VisusDataRequest, VisusDataSource, VisusDataSourceFactory
from pyvisus.display import VisusOrthogonalSlice
from pyvisus.extract import VisusAxisAlignedExtractor
from pyvisus.shared import VisusSharedDataRequest, VisusSharedBoundingBox

gValues=[]
gCurrent=0
window = None
adata = None
gFocus = None
gDataSet = None
gSamples = None
gRoot = None
winWidth = 800
winHeight= 600
gMouseX = 0
gMouseY = 0
gPressed = 0
gMouseMotion = False
gBBox = None
gModifiers = 0

def printBox(label, box):
  print label," box: ", box[0], box[1], box[2], box[3], box[4], box[5]
  return

def constructWorldBox(data_box):
  box = VisusBoundingBox()

  printBox("Input",data_box)

  scale_factor = 10.0 / (data_box[3] - data_box[0])
  
  tmp = 10.0 / (data_box[4] - data_box[1])
  if tmp < scale_factor:
    scale_factor = tmp

  tmp = 10.0 / (data_box[5] - data_box[2])
  if tmp < scale_factor:
    scale_factor = tmp

  for i in xrange(3): 
    box[i]   = -(data_box[i+3] - data_box[i]) * scale_factor / 2.0
    box[i+3] = +(data_box[i+3] - data_box[i]) * scale_factor / 2.0 

  printBox("Output",box)

  return box;


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
  glOrtho(-10,10,-10*3/4.0,10*3/4.0,-100,100)
  gluLookAt(0,0,10,0,0,-1,0,1,0)
  return


def keyboard( *args ):
  global gCurrent,gFocus,gValues
  import sys
  (key, x, y) = args

  if key == 27 or key == 'x':
    sys.exit(0)
  elif key == 'm':
    VisusXMLInterface().write("restart.xml", gRoot)
  elif key == 't':
    gCurrent += 1
    gCurrent %= len(gValues)
    gFocus = gValues[gCurrent]
    print "Focusing on ", gFocus

  return


def motion( *args ):
  global gFocus,gPressed,gMouseMotion,gMouseX,gMouseY,gModifiers
  (x, y) = args 

  if not gMouseMotion:
    return

  newX = (x-gMouseX) / (1.0 * winWidth)
  newY = (y-gMouseY) / (1.0 * winHeight)

  if gPressed == GLUT_LEFT_BUTTON:
      gRoot.rotate(newX,newY)

  elif gPressed == GLUT_MIDDLE_BUTTON:
    if gFocus is not None:
       gDerived = gFocus.__deref__()
       if gModifiers and GLUT_ACTIVE_SHIFT and isinstance(gDerived, VisusOrthogonalSlice):
         gDerived.translateRequest(newX,newY)
       else:
          gFocus.translate(newX,newY)

  elif gPressed == GLUT_RIGHT_BUTTON:
      gRoot.scale(newX,newY)

  gMouseX = x
  gMouseY = y

  glutPostRedisplay()
  return


def mouse( *args ):
  global gPressed,gMouseMotion,gMouseX,gMouseY,gModifiers
  (button, state, x, y) = args
  if state == GLUT_DOWN:
    gModifiers = glutGetModifiers()
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
  glutPostRedisplay()
  return


def createData(filename=None):
  global gBBox, gDataSet, adata 

  if filename is None: 
    import numpy
    from math import sqrt, pow
    from pyvisus.numpyconversion import VisusIncoreEncoder

    data = []
    for i in xrange(50*50*50):
       data.append(0)

    for i in xrange(50):
      for j in xrange(50):
        for k in xrange(50):
          data[k+j*50+i*50*50] = sqrt(pow(i-25.0,2) + pow(j-25.0,2) + pow(k-25.0,2))

    adata = numpy.array(data)

    encoder = VisusIncoreEncoder(50,50,50)
    encoder.field(adata)
    gDataSet = str(encoder) 
  else: 
    gDataSet = "Idx:%s" % filename
    
  # Make the data source
  print "Opening dataset(%s)" % gDataSet
  data =  VisusDataSourceFactory.make(gDataSet); 
  if not data.isValid():
    raise RuntimeError("Loaded data is not valid")

  gSamples = data.samples();
  left = VectorDouble(3)
  right= VectorDouble(3)
  data.domainBoundingBox(left,right);

  gBBox = VisusBoundingBox()
  gBBox.set(left,right);

  return

def addSlice(parent,orientation):
  global gDataSet, gFocus, gValues

  # Get Data
  left = VectorDouble(3)
  right= VectorDouble(3)

  print "Reloading dataset(%s)" % gDataSet
  source = VisusDataSourceFactory.make(gDataSet)
  if not source.isValid():
    raise RuntimeError("Problem loading data set")
  source.domainBoundingBox(left,right)

  # Compute Data Requeset
  request = VisusDataRequest()

  extent = VectorDouble(3)
  extent[0] = 0.9 * (right[0] - left[0])
  extent[1] = 0.9 * (right[1] - left[1])
  extent[2] = 0
  request.extent(extent)
  start = [ 4, 4, 4 ]
  end   = [ 1, 1, 1 ]
  request.setStrides(start,end)
  matrix = translationMatrix((left[0]+right[0])/2,
                             (left[1]+right[1])/2,
                             (left[2]+right[2])/2,
                            )
  request.transformation(matrix)

  # Create Producer
  extractor = VisusAxisAlignedExtractor.construct()
  extractor.setValue(gDataSet)
  extractor.setValue(request)

  # Create Consumer
  slice = VisusOrthogonalSlice.construct()
  slice.orientation(orientation)

  # Attach To Tree
  parent.attachSubTree(extractor)
  extractor.attachSubTree(slice)
  
  slice.inherit(VisusSharedDataRequest.typeIndex(),True)
  slice.inherit(VisusSharedBoundingBox.typeIndex(),True);
  extractor.setValue(request)
  slice.setValue(request)
  if not slice.connectInput(extractor):
      raise RuntimeError("unable to connect extractor as slice input")

  gFocus = slice

  gValues.extend([slice, extractor])

  return slice


if __name__ == "__main__":
  import sys
  filename = None

  newArgv = glutInit(sys.argv)
  for arg in sys.argv:
    if arg.startswith("--file="):
      (param,value) = arg.split("=")
      filename = value

  glutInitDisplayMode( GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH | GLUT_MULTISAMPLE )
  glutInitWindowSize(winWidth, winHeight)
  glutInitWindowPosition(200, 200)
  window = glutCreateWindow("ViSUS Orthogonal Slice Test")

  glutDisplayFunc( display )
  glutReshapeFunc( reshape )
  glutMouseFunc( mouse )
  glutKeyboardFunc( keyboard )
  glutMotionFunc( motion )
  glutIdleFunc( idle )

  createData(filename)
  box = constructWorldBox(gBBox)

  # Create the default scene graph 
  #gRoot = VisusSceneNode.construct()
  gRoot = VisusGroup.construct()
  printBox("World box",gBBox)
  if not gRoot.setValue(gBBox):
      print "Error occurred setting bbox"
  printBox("Box to map",box)
  gRoot.mapToWorldBox(box)
  gRoot.drawBoundingBox(True)
  gFocus = gRoot

  addSlice(gRoot, VISUS_ORIENTATION_YZ)

  gValues.append(gRoot)

  # Run The Main
  glutMainLoop()


