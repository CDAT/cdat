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

from pyvisus.core import VisusFont, VISUS_ORIENTATION_YZ, \
  VectorInt, VectorDouble, VisusXMLInterface, \
  translationMatrix, VisusBoundingBox, VisusGroup, VisusIsoValue, banded, VisusOpenGLState
from pyvisus.component import VisusSphereNode, VisusSceneNode, \
  VisusLabelNode, VisusColorBar
from pyvisus.data import VisusDataRequest, VisusDataSource, \
  VisusDataSourceFactory
from pyvisus.display import VisusMeshDisplay
from pyvisus.extract import VisusAxisAlignedExtractor, VisusColoredIsoSurface
from pyvisus.shared import VisusSharedDataRequest

ISO_NDX   = 0
COLOR_NDX = 1

gTime = 0
gColorMap = banded()
gCenter=[None, None, None]
gValues=[]
gCurrent=0
window = None
adata = [None, None] 
gFocus = None
gDataSet =[None, None] 
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
gIsoValueLabel = None
gMinValue = 1.0e+10
gMaxValue =-1.0e+10

def glInit():
  global winWidth, winHeight

  light1_ambient  = [ 1.0, 1.0, 1.0, 1.0 ]
  light1_diffuse  = [ 1.0, 0.9, 0.9, 1.0 ]
  light1_specular = [ 1.0, 0.7, 0.7, 1.0 ]  
  light1_position = [ -1.0, 1.0, 1.0, 0.0 ] 

  glLightfv(GL_LIGHT1, GL_AMBIENT,  light1_ambient)
  glLightfv(GL_LIGHT1, GL_DIFFUSE,  light1_diffuse)
  glLightfv(GL_LIGHT1, GL_SPECULAR, light1_specular)
  glLightfv(GL_LIGHT1, GL_POSITION, light1_position)
  glEnable(GL_LIGHT1)

  glEnable(GL_LIGHTING)
  glEnable(GL_DEPTH_TEST)
   
  glClearColor(0,0,0,0)
   
  glMatrixMode( GL_PROJECTION)
  glLoadIdentity();
  glOrtho(-10,10,-10.0*winHeight/(1.0*winWidth),10.0*winHeight/(1.0*winWidth),-100,100)
   
  glMatrixMode(GL_MODELVIEW)
  glLoadIdentity()
  return


def updateIso(isoValue):
   textValue = "IsoValue: %f" % isoValue.data()
   gIsoValueLabel.text(textValue)
   return


def printBox(label, box):
  print label," box: ", box[0], box[1], box[2], box[3], box[4], box[5]
  return


def constructWorldBox(data_box):
  box = VisusBoundingBox()

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

  return box;


def display():
  glutSetWindow(window);
  glClear (GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)

  state = VisusOpenGLState()
  state.fetchState()
  gRoot.setValue(state)

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

  elif key == '+':
    if gFocus is not None: 
      iso_value = VisusIsoValue()
      gFocus.getValue(iso_value)
      iso_value.data(iso_value.data()*1.1)
      gFocus.setValue(iso_value)
      updateIso(iso_value)
      glutPostRedisplay()

  elif key == '-':
    if gFocus is not None: 
      iso_value = VisusIsoValue()
      gFocus.getValue(iso_value)
      iso_value.data(iso_value.data()/1.1)
      gFocus.setValue(iso_value)
      updateIso(iso_value)
      glutPostRedisplay()

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
       if gModifiers and GLUT_ACTIVE_SHIFT and isinstance(gDerived, VisusColoredIsoSurface):
          gDerived.shiftRequest(newX,newY)
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
  global gTime
  import time

  if gRoot.readClearDirty():
    glutPostRedisplay()

    # Somehow first marking of dirty gets cleared without right draw
    if gTime == 0:
      gRoot.markAsDirty()
      gTime = 1
      time.sleep(0.2)
  else:
    time.sleep(0.001)
  return

def funcIso(i,j,k):
  global gCenter
  from math import sqrt, pow
  return sqrt(pow(i-gCenter[2],2) + pow(j-gCenter[1],2) + pow(k-gCenter[0],2))

def funcColor(i,j,k):
  return i + j*j*100.0 + k*2.0

def createData(func,idx):
  global gDataSet, adata, gCenter, gMinValue, gMaxValue

  import numpy
  from pyvisus.numpyconversion import VisusIncoreEncoder

  print "Create data for idx(%d)" % idx

  dimi = 50
  dimj = 50
  dimk = 50

  gCenter[0] = 0.5 * (dimi-1)
  gCenter[1] = 0.5 * (dimj-1)
  gCenter[2] = 0.5 * (dimk-1)

  data = []
  for i in xrange(dimi*dimj*dimk):
    data.append(0)

  for i in xrange(dimi):
    for j in xrange(dimj):
      for k in xrange(dimk):
        value = func(i,j,k)
        data[k + j*dimk + i*dimk*dimj] = value

	if idx == COLOR_NDX:
          if value < gMinValue:
            gMinValue = value
          if value > gMaxValue:
            gMaxValue = value

  adata[idx] = numpy.array(data)

  encoder = VisusIncoreEncoder(dimi,dimj,dimk,1)
  encoder.domain([0,0,0],[dimk,dimj,dimi])
  encoder.field(adata[idx], "density")
  gDataSet[idx] = str(encoder) 
    
  # Make the data source
  print "Opening dataset(%s)" % gDataSet[idx]
  data =  VisusDataSourceFactory.make(gDataSet[idx]); 
  if not data.isValid():
    raise RuntimeError("Loaded data is not valid")

  return data


def addIsoSurface(parent):
  global gDataSet, gFocus, gValues, gMinValue, gMaxValue

  # Get Data
  left = VectorDouble(3)
  right= VectorDouble(3)

  print "Reloading dataset(%s)" % gDataSet[ISO_NDX]
  source = VisusDataSourceFactory.make(gDataSet[ISO_NDX])
  if not source.isValid():
    raise RuntimeError("Problem loading data set")
  source.domainBoundingBox(left,right)
  
  print "right:", right[0], right[1], right[2]
  print "left: ", left[0], left[1], left[2]

  # Compute Data Requeset
  request = VisusDataRequest()

  extent = VectorDouble(3)
  extent[0] = 0.5 * (right[0] - left[0])
  extent[1] = 0.5 * (right[1] - left[1])
  extent[2] = 0.5 * (right[2] - left[2])
  print "extent: ", extent[0], extent[1], extent[2]

  request.extent(extent)
  start = [ 1, 1, 1 ]
  end   = [ 1, 1, 1 ]
  request.setStrides(start,end)
  matrix = translationMatrix((left[0]+right[0])/2,
                             (left[1]+right[1])/2,
                             (left[2]+right[2])/2,
                            )
  request.transformation(matrix)

  # Create Producer
  extractorIso = VisusAxisAlignedExtractor.construct()
  extractorIso.setValue(gDataSet[ISO_NDX])

  extractorColor = VisusAxisAlignedExtractor.construct()
  extractorColor.setValue(gDataSet[COLOR_NDX])

  # Create Consumer
  iso = VisusColoredIsoSurface.construct()

  # Create Display
  display = VisusMeshDisplay.construct()
  display.normalIndex(3)
  display.colorIndex(6)
  display.minValue(gMinValue)
  display.maxValue(gMaxValue)
  display.setValue(gColorMap)

  # Attach To Tree
  parent.attachSubTree(iso)
  iso.attachSubTree(extractorIso)
  iso.attachSubTree(extractorColor)
  iso.attachSubTree(display)

  # Connect Request
  # extractorIso.inherit(VisusSharedDataRequest.typeIndex(),True)
  # extractorColor.inherit(VisusSharedDataRequest.typeIndex(),True)
  extractorColor.setValue(request)
  extractorIso.setValue(request)

  # Connect Inputs
  if not iso.connectIso(extractorIso):
    raise RuntimeError("unable to connect extractor as iso input")
  
  if not iso.connectColor(extractorColor):
    raise RuntimeError("unable to connect extractor as color input")

  if not display.connectInput(iso):
    raise RuntimeError("unable to connect iso as display input")

  gFocus = iso 
  gFocus.drawBoundingBox(True)

  gValues.extend([iso, extractorIso, extractorColor])

  return iso 


if __name__ == "__main__":
  import sys

  newArgv = glutInit(sys.argv)

  glutInitDisplayMode( GLUT_DOUBLE | GLUT_RGBA | GLUT_DEPTH | GLUT_MULTISAMPLE )
  glutInitWindowSize(winWidth, winHeight)
  glutInitWindowPosition(200, 200)
  window = glutCreateWindow("ViSUS Colored Iso Surface Test")

  glutDisplayFunc( display )
  glutReshapeFunc( reshape )
  glutMouseFunc( mouse )
  glutKeyboardFunc( keyboard )
  glutMotionFunc( motion )
  glutIdleFunc( idle )

  glInit()

  data = createData(funcIso, ISO_NDX)
  createData(funcColor, COLOR_NDX)

  gSamples = data.samples();
  left = VectorDouble(3)
  right= VectorDouble(3)
  data.domainBoundingBox(left,right);

  gBBox = VisusBoundingBox()
  gBBox.set(left,right);

  box = constructWorldBox(gBBox)

  # Create the default scene graph 
  gRoot = VisusGroup.construct()
  if not gRoot.setValue(gBBox):
      print "Error occurred setting bbox"
  gRoot.mapToWorldBox(box)
  gRoot.drawBoundingBox(True)
  gFocus = gRoot

  isoValue = VisusIsoValue(7.0)

  iso = addIsoSurface(gRoot)
  iso.setValue(isoValue)

  font = VisusFont()
  font.fontSize(3)

  gColorBar = VisusColorBar.construct()
  gColorBar.position(-1,-0.8)
  gColorBar.setValue(gColorMap)
  axis = gColorBar.axis()
  axis.drawLegend(False)
  axis.labelFont(font)
  axis.minValue(gMinValue)
  axis.maxValue(gMaxValue)
  gColorBar.axis(axis)
  gRoot.attachSubTree(gColorBar)

  gIsoValueLabel = VisusLabelNode.construct()
  gIsoValueLabel.position(0.8,-0.9)
  gIsoValueLabel.setValue(font)
  gRoot.attachSubTree(gIsoValueLabel)

  updateIso(isoValue)

  gValues.extend([gRoot,gColorBar,gIsoValueLabel])


  # Run The Main
  glutMainLoop()


