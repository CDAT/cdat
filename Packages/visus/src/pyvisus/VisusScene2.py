#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Scene 2 - 4 cartesian slices, each showing mean and std dev for a different day
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
import pyvisus
from pyvisus.gui import *
from pyvisus.core import *
from pyvisus.VisusFunctions import *
from pyvisus.component import *
from numpy import uint8
import cdms2
import VisusCartesianSlice
import time
import Image
import VisusFourSliceScene

if __name__ == "__main__":
  """
  Scene to show the mean and standard deviation of two different days
  via colormapping onto 2D orthogonal slices. 
  """

  from sys import executable as exe
  from os.path import join,isdir,split
  
  ## Open the data
  path = "/usr/sci/ViSUS/Climate/20c3m_derived/"
  path2 = "/usr/sci/ViSUS/Climate/20c3m/"
  
  mean = "atm/mo/tas/mean/mean_1900_1_1900_12.nc"
  stdd = "atm/mo/tas/std/std_1900_1_1900_12.nc"
  mini = "atm/mo/tasmin/bccr_bcm2_0/run1/tasmin_A1_1.nc"
  maxi = "atm/mo/tasmax/bccr_bcm2_0/run1/tasmax_A1_1.nc"

  meanfile = cdms2.open(path+mean)
  stddfile = cdms2.open(path+stdd)
  minifile = cdms2.open(path2+mini)
  maxifile = cdms2.open(path2+maxi)

  # Read the surface temperature variable and load it into memory
  meanTas = meanfile('tas', 
                     time = ('1900-1-16 0:0:0.0', '1900-12-16 0:0:0.0'),
                     latitude = (-90, 90),
                     longitude = (-180, 180),
                     squeeze = 0, 
                     order = '012' )
  stddTas = stddfile('tas', 
                     time = ('1900-1-16 0:0:0.0', '1900-12-16 0:0:0.0'),
                     latitude = (-90, 90),
                     longitude = (-180, 180),
                     squeeze = 0, 
                     order = '012' )  
  miniTas = minifile('tasmin', 
                       time = ('1900-1-16 0:0:0.0', '1900-12-16 0:0:0.0'),
                       latitude = (-90, 90),
                       longitude = (-180, 180),
                       squeeze = 0, 
                       order = '012' )


  maxiTas = maxifile('tasmax', 
                     time = ('1900-1-16 0:0:0.0', '1900-12-16 0:0:0.0'),
                     latitude = (-90, 90),
                     longitude = (-180, 180),
                     squeeze = 0, 
                     order = '012' )

 
  VisusFourSliceScene.FourSliceScene(meanTas, stddTas, miniTas, maxiTas)
