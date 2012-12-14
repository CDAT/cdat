"""
Test conservative regridding of strange grids
NorESM1 Grid

$Id: testNorESM1Grid.py 2466 2012-10-05 20:30:38Z dkindig $
"""
import cdms2
import numpy as P
import ESMP
import glob
import os
import unittest

ESMP.ESMP_LogSet(True)

class testMPIGrid(unittest.TestCase):
  def setUp(self):
    dir = "../cdat_data/"
    filename = dir + "so_Omon_NorESM1-M_historicalNat_r1i1p1_185001-185312_2timesteps.nc"
    g = cdms2.open(filename)
    self.so = g('so')[0,0,...]
    gLat = cdms2.createGaussianAxis(64)
    deltaLon = (360/128.)
    gLon = cdms2.createUniformLongitudeAxis(0, 128, deltaLon)
    self.gaussGrid = cdms2.grid.createGenericGrid(gLat[:], gLon[:], 
                                         gLat.getBounds(),
                                         gLon.getBounds())

  def test1_MPIGrid(self):

    soN = self.so.regrid(self.gaussGrid, rt = 'esmf', rm = 'conserve',
                        coordSys = 'deg', periodicity = 1)
    self.so.toVisit('mpi.vsh5','Vs')
    soN.toVisit('gg.vsh5', 'Vs')

if __name__ == '__main__':
    ESMP.ESMP_Initialize()
    print ""
    suite = unittest.TestLoader().loadTestsFromTestCase(testMPIGrid)
    unittest.TextTestRunner(verbosity = 2).run(suite)
