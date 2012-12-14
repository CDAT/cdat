"""
Test conservative regridding of strange grids
MPI grid will regrid IFF the bounds are reordered to

1212
0303 

from 

3232
0101

I did this manually in mvCdmsRegrid.py

$Id: testMPIGrid.py 2472 2012-10-12 13:10:52Z pletzer $
"""
import cdms2
import numpy as P
import ESMP
import glob
import os
import unittest
import sys

ESMP.ESMP_LogSet(True)

class TestGrid(unittest.TestCase):

  def setUp(self):
    dir = sys.prefix + "/sample_data/"
    filename = dir + "so_Omon_MPI-ESM-LR_1pctCO2_r1i1p1_185001-185912_2timesteps.nc"
    g = cdms2.open(filename)
    self.so = g('so')[0,0,:,:254]
    gLat = cdms2.createGaussianAxis(64)
    deltaLon = (360/128.)
    gLon = cdms2.createUniformLongitudeAxis(0, 128, deltaLon)
    self.gaussGrid = cdms2.grid.createGenericGrid(gLat[:], gLon[:], 
                                         gLat.getBounds(),
                                         gLon.getBounds())

  def test1(self):
    soN = self.so.regrid(self.gaussGrid, rt = 'esmf', rm = 'conserve',
                         coordSys = 'deg', periodicity = 1,
                         fixSrcBounds=False)
    self.so.toVisit('MPI_so.vsh5','Vs')
    soN.toVisit('MPI_so_interp.vsh5', 'Vs')

if __name__ == '__main__':
    ESMP.ESMP_Initialize(logkind = ESMP.ESMP_LOGKIND_NONE)
    #ESMP.ESMP_LogSet(False)
    print ""
    suite = unittest.TestLoader().loadTestsFromTestCase(TestGrid)
    unittest.TextTestRunner(verbosity = 2).run(suite)
