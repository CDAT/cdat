#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Scene 0 - colormapped sphere with a line plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
import pyvisus
from pyvisus.gui import *
from pyvisus.core import *
from pyvisus.VisusFunctions import *
from pyvisus.display import VisusOrthogonalSlice,VisusEarthNode,VisusIndexedDisplay, VisusMeshDisplay
from pyvisus.extract import VisusAxisAlignedExtractor,VisusIsoSurface
from numpy import uint8
import cdms2
import VisusRectangleScene

if __name__ == "__main__":
  " Main function to colormap a sphere based on a dataset. "

  from sys import executable as exe
  from os.path import join,isdir,split
  
  # The LOCAL path to the data
  filebaseSCI = "/usr/sci/ViSUS/Climate/20c3m/atm/mo/"
  filebaseSCIDerived  = "/usr/sci/ViSUS/Climate/20c3m_derived/atm/mo/"
  filebaseBremer = "/Users/bremer5/Code/visus2.0/python/data/20c3m/atm/mo/"
  filebaseYan = "/opt/data/"
  
  # The full file paths to mean and std deviation
  filebase = filebaseSCIDerived
  # filebase = filebaseBremer
  fileMean = cdms2.open(join(filebase, "tas/mean/mean_1900_1_1900_12.nc"))
  fileStd  = cdms2.open(join(filebase, "tas/std/std_1900_1_1900_12.nc"))
  
  # The full file paths to the cloudiness variable
  filebaseCl = filebaseSCI
  #filebaseCl = filebaseBremer
  print(join(filebaseCl, "cl/bccr_bcm2_0/run1/cl_A1_1.nc"))
  fileCl = cdms2.open(join(filebaseCl, "cl/bccr_bcm2_0/run1/cl_A1_1.nc"))

  # Read in the mean, standard deviation and cloudiness variables
  cl = fileCl('cl', 
              time = ('1870-1-16 0:0:0.0', '1870-12-16 0:0:0.0'),
              latitude = (-90, 90),
              longitude = (-180, 180),
              squeeze = 0, 
              order = '012' )
  
  mean = fileMean('tas', 
                  time = ('1900-1-16 0:0:0.0', '1900-12-16 0:0:0.0'),
                  latitude = (-90, 90),
                  longitude = (-180, 180),
                  squeeze = 0, 
                  order = '012' )

  std = fileStd('tas', 
              time = ('1900-1-16 0:0:0.0', '1900-12-16 0:0:0.0'),
              latitude = (-90, 90),
              longitude = (-180, 180),
               squeeze = 0, 
              order = '012' )

  scene = VisusRectangleScene.RectangleScene(mean, cl)
