#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Scene 0 - 2D ortho slices
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
import pyvisus
from pyvisus.gui import *
from pyvisus.core import *
from pyvisus.component import *
from numpy import uint8
import cdms2
import time
import VisusCartesianSlice
import VisusFunctions

if __name__ == "__main__":
  " Main function to colormap a 2D slice based on a dataset. "

  from sys import executable as exe
  from os.path import join,isdir,split
  
  # open some dataset
  filebase = "/usr/sci/ViSUS/Climate/20c3m/atm/mo/"
  #filebase = "/Users/bremer5/Code/visus2.0/python/data/20c3m/atm/mo/"
  #filebase = "/opt"
  modelbase = "cl/ncar_ccsm3_0/run1/"
  dataset = "cl_A1.20C3M_1.CCSM.atmm.1870-01_cat_1879-12.nc"
  file = cdms2.open(filebase+modelbase+dataset)

  #-!-!- Print out the variables in the file -!-!-#
  #print file.listvariables()
  #-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-#

  # Read the 'cl' variable and load it into memory
  cl = file( 'cl', 
             time = ('1870-1-16 0:0:0.0', '1870-12-16 0:0:0.0'),
             latitude = (-90, 90),
             longitude = (-180, 180),
             squeeze = 0, 
             order = '012' )

  #-!-!- Print out the 'cl' variable information -!-!-#
  #cl.info()
  #print cl.shape
  #-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-!-#
  
  # Create the 2D slice
  slice1 = VisusCartesianSlice.CartesianSlice(cl)
  slice2 = VisusCartesianSlice.CartesianSlice(cl, dict(), slice1.getRoot(), False)
  
  # Move and scale the slices so we can see them
  slice1.scaleSlice(0.0, -0.25)
  slice1.translateSlice(0, -12)

  slice2.scaleSlice(0.0, -0.25)
  slice2.translateSlice(0, 12)

  # Change the colormap
  slice1.setColorMap(bry())
  slice2.setColorMap(bry())

  # create a colorbar (use just one for the collection)
  slice1.createColorBar()

  # Change the data on the slices
  slice2.moveDataRequest(0.2)

  # Animate the data
  descriptors = []
  attributes = []
  attribute = dict()
  for i in range(0,cl.shape[0]):
    d = VisusFunctions.constructDataDescriptor(cl[i],attribute)
    descriptors.append(d)

  for x in range(1):
    for i in range(len(descriptors)):  
      d = descriptors[i]
      old = d
      slice1.getExtractor().setValue(d)
      slice2.getExtractor().setValue(d)
      time.sleep(1)

  print "GoodBye!"
