#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Scene 1 - 1 cartesian slice
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
import pyvisus
from pyvisus.gui import *
from pyvisus.core import *
from pyvisus.VisusFunctions import *
from numpy import uint8
import cdms2
import VisusSliceCollection
import time

if __name__ == "__main__":
  " Main function to show 3 cartesian slices from a single dataset. "

  from sys import executable as exe
  from os.path import join,isdir,split

  # The LOCAL path to the data
  filebaseSCI = "/usr/sci/ViSUS/Climate/20c3m/atm/mo/"
  filebaseBremer = "/Users/bremer5/Code/visus2.0/python/data/20c3m/atm/mo/"
  filebaseYan = "/opt/data/"
    
  # The full file paths to the cloudiness variable
  filebase = filebaseSCI
  #filebase = filebaseBremer
  print(join(filebase, "cl/bccr_bcm2_0/run1/cl_A1_1.nc"))
  fileCl = cdms2.open(join(filebase, "cl/bccr_bcm2_0/run1/cl_A1_1.nc"))
  fileTas = cdms2.open(join(filebase, "tas/bccr_bcm2_0/run1/tas_A1_1.nc"))

  # Read in the variables
  tas = fileTas('tas', 
                time = ('1870-1-16 0:0:0.0', '1870-12-16 0:0:0.0'),
                latitude = (-90, 90),
                longitude = (-180, 180),
                squeeze = 0, 
                order = '012' )

  cl = fileCl('cl', 
            time = ('1870-1-16 0:0:0.0', '1870-12-16 0:0:0.0'),
            latitude = (-90, 90),
            longitude = (-180, 180),
            squeeze = 0, 
            order = '012' )
    
  # Create a cartesian slice
  threeSlices = VisusSliceCollection.SliceCollection(tas)
  threeSlices.addIsoSurface( cl, 0 )

  # Animate the data changing on the slices
  #for i in range(1):
  #  print "i: ", i
  #  threeSlices.animate(i)
  #  time.sleep(1)

  print "GoodBye!"
