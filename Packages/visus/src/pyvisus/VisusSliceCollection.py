import pyvisus
from pyvisus.core import *
from pyvisus.gui import *
from pyvisus.data import *
from pyvisus.component import * 
from pyvisus.shared import VisusSharedDataRequest
import VisusFunctions
import VisusCartesianSlice
import time

#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
# Class to hold x, y, z tuples with accessors
#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
class XYZTuple:
  """
  Class to hold x, y, and z tuples
  """
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Default position (0, 0)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def __init__( self, x = 0, y = 0, z = 0 ):
    self._x = x
    self._y = y
    self._z = z

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Get the x position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def x( self ):
    return self._x

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Get the y position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def y( self ):
    return self._y

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Get the z position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def z( self ):
    return self._z

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Set the x position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setX( self, x ):
    self._x = x

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Set the y position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setY( self, y ):
    self._y = y

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Set the z position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setZ( self, z ):
    self._z = z

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Set the xyz position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setXYZ( self, x, y, z ):
    self._x = x
    self._y = y
    self._z = z


# $-$-$ DEFAULT POSITIONING FOR 3 SLICES $-$-$ #
defaultSlicePositions = []
#defaultSlicePositions.append(XYZTuple(0, 0.0, 0.0))  
defaultSlicePositions.append(XYZTuple(0, -400, 0.0))  
defaultSlicePositions.append(XYZTuple(0,    0, 0.1))  
defaultSlicePositions.append(XYZTuple(0,  400, 0.2))  


#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
# Class to create a collection of cartesian slices and an associated colorbar
#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
class SliceCollection:
  """
  Class to have a collection of cartesian slices and a colorbar
  """

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Construct the collection of slices (default 3 slices), and a colorbar
  # Default settings add 3 slices with a scale of -0.5
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def __init__( self, dataArray=[], usrAttribs=dict(), 
                positionArray = defaultSlicePositions, scale = XYZTuple(0.0,0.0,0.0),
                root=VisusGroup.construct(), newWindow = True):

    print "dataArray: ", len(dataArray)

    # Create a root node
    self._root = root
    self._root.drawBoundingBox(False)  
    
    # Create the list of slices
    self._sliceList = []
    for i in range(len(dataArray)):
      self._sliceList.append(VisusCartesianSlice.CartesianSlice(dataArray[i], usrAttribs, self._root, False))
      
    # Create the drawing window
    if newWindow:
      createWindow(self._root)

    # Position and scale the slices based on the passed in positions
    for i in range(len(self._sliceList)):
      print "slice range i :", i
      self.scaleSlice(i, scale)
      self.translateSlice(i, positionArray[i] )

    # !-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-! #
    # TODO: make the positioning automatic based on the bounding box of the 1st slice
    # Get the bounding box of the first slice
    #bbox = self._sliceList[0].getBoundingBox()
    #print "box: ", bbox[0], " ", bbox[1], " ", bbox[2], " " , bbox[3], " ", bbox[4], " ", bbox[5]
    #width = bbox[3] - bbox[0]
    #height  = bbox[4] - bbox[1] 
    #print "height: ", height, " width: ", width 
    # !-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-! #

    # Create a colorbar for the collection
    self.createColorBar()
    self.setColorBarPosition(1.0, -0.45)
    
    # Move the data request based on the parameter position
    for i in range(len(self._sliceList)):
      self.changeDataRequest(i, positionArray[i].z())
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Get the root of the collection
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getRoot( self ):
    return self._root

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Get the slice requested by the parameter
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getSlice( self, i ):
    return self._sliceList[i]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Scale the specified slice
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def scaleSlice( self, i, scale = XYZTuple(0, 0, 0) ):
  #def scaleSlice( self, i, scale ):
    print "scale: " , scale.x(), scale.y()
    self._sliceList[i].scaleSlice(scale.x(), scale.y())

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Translate the specified slice
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def translateSlice( self, i, trans = XYZTuple() ):
    self._sliceList[i].translateSlice(trans.x(), trans.y())
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Create a colorbar for this slice
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def createColorBar( self ):
    
    # Create a colorbar for the slice 
    self._colorBar = VisusColorBar.construct()
    self._colorBar.orientation(BB_VERTICAL)

    # Get the axis of the colorbar
    axis = self._colorBar.axis()

    # Get the attributes of the data
    attribs = self._sliceList[0].getAttribs()

    # If we found a variable name we use it as label
    if "name" in attribs:
      axis.legendText(attribs["name"].replace("_", " "))
    else:
      axis.legendText("")
 
    # If we found a range we use it as well. 
    if "range" in attribs:
      axis.minValue(float(attribs["range"][0]))
      axis.maxValue(float(attribs["range"][1]))
  
    font = VisusFont()
    font.fontSize(5)
    axis.legendFont(font)
    axis.labelFont(font)
    self._colorBar.axis(axis)
  
    # Finally, attach the color bar to the slice and make sure it
    # inherits its colormap from there
    self._sliceList[0].getOrthoSlice().attachSubTree(self._colorBar,True)
    self._colorBar.inherit(8,True)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Return the colorbar
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def getColorBar( self ):
    return self._colorBar

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Set the position of the colorbar
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def setColorBarPosition( self, x, y ):
    self._colorBar.position( x, y )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Change the data on the slices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def changeDataRequest( self, i, z ):
    self._sliceList[i].moveDataRequest( z )
 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Animate the data on the slices by updating the time
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def animate( self, i, t = VisusGlobalTime() ):
    for j in range(len(self._sliceList)):
      self._sliceList[j].getExtractor().getValue(t)
      t.inc()
      self._sliceList[j].getExtractor().setValue(t)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Add an isosurface
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def addIsoSurface( self, isoData, i=0 ):
    
    # Add an isosurface to the ith slice
    self._sliceList[i].addIsoSurface(isoData)
