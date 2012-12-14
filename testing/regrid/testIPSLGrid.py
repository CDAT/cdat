"""
Test conservative regridding of strange grids
IPSL Grid

$Id: testIPSLGrid.py 2472 2012-10-12 13:10:52Z pletzer $
"""
import cdms2
import numpy as P
import ESMP
import glob
import os
import unittest

ESMP.ESMP_LogSet(True)

class TestGrid(unittest.TestCase):

  def setUp(self):
    dir = "../cdat_data/"
    filename = dir + "so_Omon_IPSL-CM5A-LR_1pctCO2_r1i1p1_185001-189912_2timesteps.nc"
    g = cdms2.open(filename)
    self.so = g('so')[0,0,...]
    gLat = cdms2.createGaussianAxis(64)
    deltaLon = (360/128.)
    gLon = cdms2.createUniformLongitudeAxis(0, 128, deltaLon)
    self.gaussGrid = cdms2.grid.createGenericGrid(gLat[:], gLon[:], 
                                         gLat.getBounds(),
                                         gLon.getBounds())

  def test1(self):
    soN = self.so.regrid(self.gaussGrid, rt = 'esmf', rm = 'conserve',
                        coordSys = 'deg', periodicity = 1,
                        fixSrcBounds=True)
    self.so.toVisit('IPSL_so.vsh5','Vs')
    soN.toVisit('IPSL_so_interp.vsh5', 'Vs')

if __name__ == '__main__':
    ESMP.ESMP_Initialize()
    print ""
    suite = unittest.TestLoader().loadTestsFromTestCase(TestGrid)
    unittest.TextTestRunner(verbosity = 2).run(suite)
