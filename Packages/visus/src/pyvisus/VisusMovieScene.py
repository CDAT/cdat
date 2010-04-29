import pyvisus
from pyvisus.core import *
from pyvisus.gui import *
from pyvisus.data import *
from pyvisus.component import * 
from pyvisus.shared import VisusSharedDataRequest
import VisusFunctions
import VisusColorMapSphere
import VisusCartesianSlice
import time

#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
# Class to encapsulate the default movie scene
#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
class MovieScene:
  """
  Class to encapsulate the default movie scene
  """
    
  _interface_info = """
Select up to 3 variables they will define

Variable 1: Color map on the earth and data used for slices
Variable 2: Height data on the earth
Variable 3: Iso-surface around the earth
"""
  _min_variables = 1
  _max_variables = 3

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Construct the sphere, slice collection and labels
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def __init__( self, dataArray1, dataArray2=[], dataArray3=[] ):

    # Create the root node
    self.root=VisusGroup.construct()

    # Create the 2D slices
    self._slice1 = VisusCartesianSlice.CartesianSlice(dataArray3, "Slice at 0.25",dict(),self.root,False)
    self._slice2 = VisusCartesianSlice.CartesianSlice(dataArray3, "Slice at 0.5", dict(), self.root, False)
    self._slice3 = VisusCartesianSlice.CartesianSlice(dataArray3, "Slice at 0.75", dict(), self.root, False)

    self._slice1.moveDataRequest(-6);
    self._slice3.moveDataRequest(+6);

    # Create the earth node
    
    self.group=VisusGroup.construct()
    self.root.attachSubTree(self.group)
    
    self._earth = VisusColorMapSphere.ColorMapSphere(dataArray1, dataArray2, dataArray3, 1, None,
                                                     dict(), self.group, False)

    self.root.propagateDownwards(VISUS_GLOBALTIME_TYPEID)

    bbox = VisusBoundingBox()
    self.group.getValue(bbox)
    self.root.setValue(bbox)
    createWindow(self.root)
    
    scale = scaleMatrix(0.5,0.5,0.5);
    m = VisusTransformation3D()
    self._earth._earth.getValue(m)
    m = m * scale
    
    self._earth._earth.setValue(m)
    self._earth._bgEarth.setValue(m)
    
    # Change the colormap
    self._slice1.setColorMap(bry())
    self._slice2.setColorMap(bry())
    self._slice3.setColorMap(bry())

    scale = scaleMatrix(0.3,0.3,0.3);

    self._slice1._extractor.setValue(scale);
    self._slice2._extractor.setValue(scale);
    self._slice3._extractor.setValue(scale);
    # Scale the slices
    #slice1.scaleSlice(-0.5, -0.5)
    #slice2.scaleSlice(-0.5, -0.5)
    #slice3.scaleSlice(-0.5, -0.5)

    # Translate the slices
    self._slice1.translateSlice(16, -20)
    self._slice2.translateSlice(16, 0)
    self._slice3.translateSlice(16, 20)

    # Move the titles of the slices
    self._slice1.getTitle().position(0.3, 0.9)
    self._slice2.getTitle().position(0.3, 0.23)
    self._slice3.getTitle().position(0.3,-0.45)

    # FReeze the slices
    self._slice1.freeze()
    self._slice2.freeze()
    self._slice3.freeze()

    # Create a colorbar for the slice 
    self._colorBar = VisusColorBar.construct()
    self._colorBar.orientation(BB_VERTICAL)

    # Get the axis of the colorbar
    axis = self._colorBar.axis()

    # Get the attributes of the data
    attribs = self._slice1.getAttribs()
    
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
    font.fontSize(8)
    font.fontStyle(VISUS_TEXTURE)
    axis.legendFont(font)
    axis.legendOffset(1000)
    #font.fontStyle(VISUS_PIXMAP)
    axis.labelFont(font)
    self._colorBar.axis(axis)
  
    # Finally, attach the color bar to the slice and make sure it
    # inherits its colormap from there
    self._slice1.getOrthoSlice().attachSubTree(self._colorBar,True)
    self._colorBar.inherit(8,True)
    self._colorBar.position(1.15, -0.45)

    self.group.translate(-17, 0)
    self._earth.getColorBar().translate(-.06, 0)
    self._earth.getTitle().translate(-0.075, 0)

    gVisusGuiMaster.window(0).setFocus(self.group)
