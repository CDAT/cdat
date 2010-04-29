import pyvisus
from pyvisus.gui import *
from pyvisus.core import *
from pyvisus.VisusFunctions import *
from pyvisus.component import *
from numpy import uint8
import cdms2
import VisusCartesianSlice
import time

#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
# Class to show four slices
#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
class FourSliceScene:
  """
  Class to show four slices
  """
  _interface_info = """
Select four variables to be shown as slices 

Variable 1: Color map on the earth
Variable 2: Height data on the earth
Variable 3: Iso-surface around the earth
Variable 4: Iso-surface around the earth
"""
  _min_variables = 4
  _max_variables = 4

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Construct the sphere, slice collection and labels
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def __init__( self, meanTas, stddTas, miniTas, maxiTas ):
     
    # Create the root node
    root=VisusGroup.construct()

    # Create the cartesian slice for the mean and std dev data
    meanSlice = VisusCartesianSlice.CartesianSlice(meanTas, " ", dict(), root, False) 
    stddSlice = VisusCartesianSlice.CartesianSlice(stddTas, " ", dict(), root, False) 
    miniSlice = VisusCartesianSlice.CartesianSlice(miniTas, " ", dict(), root, False) 
    maxiSlice = VisusCartesianSlice.CartesianSlice(maxiTas, " ", dict(), root, False) 
    
  

    # Set the colormap for the temperature slices
    meanSlice.setColorMap(smooth_rich())
    miniSlice.setColorMap(smooth_rich())
    maxiSlice.setColorMap(smooth_rich())
    
    # Set the colormap for the std slice
    stddSlice.setColorMap(ice())
    
    # Create a font for the scene
    font = VisusFont()
    font.fontColor(255, 255, 255)

    # Create a colorbar for the temperature slices
    colorBar = VisusColorBar.construct()
    axis = colorBar.axis()
    attribs = meanSlice.getAttribs()
    axis.legendText("Air Temperature")
    if "range" in attribs:
      axis.minValue(float(attribs["range"][0]))
      axis.maxValue(float(attribs["range"][1]))  
    font.fontSize(9)
    axis.legendFont(font)
    axis.labelFont(font)
    colorBar.axis(axis)
    colorBar.position( -1.22, -0.825)
    root.attachSubTree(colorBar)
    colorBar.setValue(meanSlice.getColorMap())

    # Create a colorbar for the standard deviation slice
    colorBar = VisusColorBar.construct()
    axis = colorBar.axis()
    attribs = stddSlice.getAttribs()
    # axis.legendText("Variance of Air Temperature")
    if "range" in attribs:
      axis.minValue(float(attribs["range"][0]))
      axis.maxValue(float(attribs["range"][1]))  
    font.fontSize(9)
    axis.legendFont(font)
    axis.legendText("Variance")
    axis.labelFont(font)
    colorBar.axis(axis)
    colorBar.position( 0, -0.6 )
    root.attachSubTree(colorBar)
    colorBar.setValue(stddSlice.getColorMap())
    
    # Create labels for the scene
    font.fontSize(9);
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
    font.fontSize(15)
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
    miniSlice.scaleSlice(scaleXY, scaleXY) 
    meanSlice.scaleSlice(scaleXY, scaleXY)
    maxiSlice.scaleSlice(scaleXY, scaleXY)
    stddSlice.scaleSlice(-0.1, -0.1)
    
    # Translate the slices
    miniSlice.translateSlice(-23, -20.5)
    meanSlice.translateSlice(-23,  -3.5)
    maxiSlice.translateSlice(-23,  13.5)
    stddSlice.translateSlice(16,  0)


  # Freeze the slices
    meanSlice.freeze()
    stddSlice.freeze()
    miniSlice.freeze()
    maxiSlice.freeze()
