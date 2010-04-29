import pyvisus
from pyvisus.gui import *
from pyvisus.core import *
from pyvisus.VisusFunctions import *
from pyvisus.component import *
from pyvisus.display import *
from numpy import uint8
import cdms2
import VisusCartesianSlice
import time

#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
# Class to show the data in a rectangle
#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
class RectangleScene:
  """
  Class to show the data on its cartesian grid
  """
  _interface_info = """
Select up to 3 variables they will define

Variable 1: Color map for the cartesian slices
Variable 2: Iso-surface on the earth
"""
  _min_variables = 1
  _max_variables = 2

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Construct the scene
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def __init__( self, data, isoData=[] ):
     
    # Create the root node
    root=VisusGroup.construct()

    # Create the cartesian slice for the data
    slice = VisusCartesianSlice.CartesianSlice(data, " ", dict(), root, False) 

    if(len(isoData) != 0):
      slice.addIsoSurface( isoData, 0)

    # Create a heightfield
    height = VisusHeightField.construct()
    height.connectHeight(slice.getExtractor())
    height.connectColor(slice.getExtractor())
    height.setValue(smooth_rich())
    height.orientation(VISUS_ORIENTATION_XZ)
    root.attachSubTree(height)

    # Set the colormap for the temperature slices
    slice.setColorMap(bgry())
        
    # Create a font for the scene
    font = VisusFont()
    font.fontColor(255, 255, 255)

    # Create a colorbar for the temperature slices
    self._colorBar = VisusColorBar.construct()
    axis = self._colorBar.axis()
    attribs = slice.getAttribs()
    axis.legendText("Air Temperature")
    if "range" in attribs:
      axis.minValue(float(attribs["range"][0]))
      axis.maxValue(float(attribs["range"][1]))  
    font.fontSize(4)
    axis.legendFont(font)
    axis.labelFont(font)
    self._colorBar.axis(axis)
    self._colorBar.position( -1.22, -0.825)
    root.attachSubTree(self._colorBar)
   # colorBar.setValue(slice.getColorMap())
   # colorBar.inherit(8, True)




    # Create labels for the scene
    font.fontSize(5);
    minilabel = VisusLabelNode.construct()
    minilabel.text("Minimum")
    minilabel.setValue(font)
    minilabel.position( -0.95, 0.92)
    root.attachSubTree(minilabel)
    
    meanlabel = VisusLabelNode.construct()
    meanlabel.text("Mean")
    meanlabel.setValue(font)
    meanlabel.position( -0.85, 0.345)
    root.attachSubTree(meanlabel)
    
    maxilabel = VisusLabelNode.construct()
    maxilabel.text("Maximum")
    maxilabel.setValue(font)
    maxilabel.position( -0.95, -0.225)
    root.attachSubTree(maxilabel)
    
    
    scenelabel = VisusLabelNode.construct()
    font.fontSize(7)
    scenelabel.text("Statistics of Air Temperature")
    scenelabel.setValue(font)
    scenelabel.position(-0.1,0.75)
    root.attachSubTree(scenelabel)
    
    scenelabel2 = VisusLabelNode.construct()
    scenelabel2.text("January 1, 1900")
    scenelabel2.setValue(font)
    scenelabel2.position(0.2,0.65)
    root.attachSubTree(scenelabel2)
    
    # Create the new window
    window = createWindow(root)
    
    # Scale the slices
    scaleXY = -0.45
    #slice.scaleSlice(scaleXY, scaleXY) 
    
    # Translate the slices
    #slice.translateSlice(-23, -20.5)

    # Freeze the slices
   # slice.freeze()
