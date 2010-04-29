import pyvisus
from pyvisus.core import *
from pyvisus.gui import *
from pyvisus.data import *
from pyvisus.component import * 
from pyvisus.shared import VisusSharedDataRequest
from pyvisus.display import VisusLineGraph,createGraphData
import VisusFunctions
import VisusColorMapSphere
import VisusCartesianSlice
import time
from math import pi,sin
import numpy

#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
# Class to show the earth with a plot
#~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~=~#
class EarthPlotScene:
  """
  Class to show the earth with a plot
  """
  _interface_info = """
Select up to 3 variables they will define

Variable 1: Color map on the earth
Variable 2: Height data on the earth
Variable 3: Iso-surface around the earth
"""
  _min_variables = 1
  _max_variables = 3

    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Construct the sphere, slice collection and labels
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  def __init__( self, dataArray1, dataArray2=[], dataArray3=[] ):
    
    # Create the line graph
    graph = VisusLineGraph.construct()
    graph.numberOfLines(1)
    dataFilter = VisusFilter()
    dataFilter(dataArray, data)
    

    #data = createGraphData(20)
    #for i in xrange(0,20):
    #  data[2*i] = 2*pi*i / 19.0;
    #  data[2*i+1] = sin(data[2*i])
    graph.loadData(data,0)
    createWindow(graph)
   
    #graph.translate(0.05, 0)

    # Create the earth node
   # earth = VisusColorMapSphere.ColorMapSphere(dataArray1, dataArray2, dataArray3, 1)
   # earth.getRoot().attachSubTree(graph)
  
    #earth.translateEarthNode(-17, 0)
    #earth.getColorBar().translate(-.06, 0)
    #earth.getTitle().translate(-0.075, 0)
