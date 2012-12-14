"""
$Id: testReverseOrderInterp.py 2405 2012-08-01 20:00:37Z dkindig $

Test xy -> yx order interpolation and visa versa.

"""

import cdms2
import regrid2
from regrid2 import esmf
from openCreateData import dataNoPeri
import unittest
import ESMP
import numpy

GenericRegrid = regrid2.mvGenericRegrid.GenericRegrid

import re

def makeGrid(nx,ny, order):
    dims = (nx, ny)
    xbot, xtop, ybot, ytop = 0, 359, -89, 89
    x = numpy.linspace(xbot, xtop, nx)
    y = numpy.linspace(ybot, ytop, ny)

    theGrid = cdms2.grid.createUniformGrid(ybot, ny, (2*ytop)/float(ny-1),
                                      xbot, nx, xtop/float(nx-1),
                                      order = order)

    if order == 'xy':
        xx = numpy.outer(x, numpy.ones(ny))
        yy = numpy.outer(numpy.ones(nx), y)
        axisList = [theGrid.getLongitude(), theGrid.getLatitude()]
    else:
        xx = numpy.outer(numpy.ones(ny), x)
        yy = numpy.outer(y, numpy.ones(nx))
        axisList = [theGrid.getLatitude(), theGrid.getLongitude()]
      
    theData = xx + yy 

    # Make a cdms2 variable
    cdmsVar = cdms2.createVariable(theData, axes = axisList, 
                                   grid = theGrid,
                                   id = 'TestOrderInterp')

    return cdmsVar

def makeXYGridList(grid):
    if grid.getOrder() == 'yx':
        sG = grid.toCurveGrid()
        return sG.getLatitude()[:], sG.getLongitude()[:]
    else:
        sG = grid.toCurveGrid()
        return sG.getLatitude()[:], sG.getLongitude()[:]
        lat = grid.getLatitude()
        lon = grid.getLongitude()
        lats = numpy.outer(numpy.ones(len(lon)), lat)
        lons = numpy.outer(lon, numpy.ones(len(lat)))
        return lats, lons


class TestOrder(unittest.TestCase):
    def setUp(self):
        data53yx = makeGrid(5,3, 'yx')
        data53xy = makeGrid(5,3, 'xy')
        data64yx = makeGrid(6,4, 'yx')
        data64xy = makeGrid(6,4, 'xy')

        self.data = [data53yx, data53xy, data64yx, data64xy]
        self.srcdata = [data53yx, data64yx]
        self.Tools = ['libcf', 'esmf']

    def test1_varRegrid(self):
        regridTool = self.Tools[0]
        print '\nUsing Tool', regridTool
        for srcData in self.data:
            srcGrid = srcData.getGrid()
            srcOrder = srcGrid.getOrder()
            for dstData in self.data:
                dstGrid = dstData.getGrid()
                dstOrder = dstGrid.getOrder()
                interpData = srcData.regrid(dstGrid, 
                                         regridTool = regridTool)
                self.assertEqual(interpData.getOrder(), str(dstOrder))
                self.assertEqual(str(interpData.shape), str(dstGrid.shape))
                interpInterpData = srcData.regrid(srcGrid, 
                                         regridTool = regridTool)
                self.assertEqual(interpInterpData.getOrder(), str(srcOrder))
                self.assertEqual(str(interpInterpData.shape), str(srcGrid.shape))

                result = abs(srcData - interpInterpData) < 1e-3
                result1 = abs(dstData - interpData) < 1e-3
                self.assertTrue(numpy.all(result))
                self.assertTrue(numpy.all(result1))

    def test2_GenericRegrid(self):
        regridTool = self.Tools[1]
        print '\nUsing Tool', regridTool
        coordSys = 'Cartesian'
        for srcData in self.data:
            srcOrder = srcData.getOrder()
            srcGrid = makeXYGridList(srcData.getGrid())
            for dstData in self.data:
                dstGrid = makeXYGridList(dstData.getGrid())
                dstOrder = dstData.getOrder()
                interpData = numpy.ones(dstData.shape)
                ro0 = GenericRegrid([numpy.array(g) for g in srcGrid], 
                                    [numpy.array(g) for g in dstGrid], 
                                    srcData.dtype, 'linear', regridTool,
                                    coordSys = coordSys)
                ro0.computeWeights()
                ro0.apply(srcData.data, interpData)
                interpInterpData = numpy.ones(srcData.shape)
                ro1 = GenericRegrid([numpy.array(g) for g in dstGrid], 
                                    [numpy.array(g) for g in srcGrid], 
                                    srcData.dtype, 'linear', regridTool,
                                    coordSys = coordSys)
                ro1.computeWeights()
                ro1.apply(interpData, interpInterpData)

                self.assertEqual(str(interpData.shape), str(dstGrid[0].shape))
                self.assertEqual(str(interpInterpData.shape), str(srcGrid[0].shape))

                result = abs(srcData - interpInterpData) < 1e-3
                result1 = abs(dstData - interpData)
                self.assertTrue(numpy.all(result))

if __name__ == '__main__':
    print "" # Spacer
    ESMP.ESMP_Initialize()
    ESMP.ESMP_LogSet(True)
    suite = unittest.TestLoader().loadTestsFromTestCase(TestOrder)
    unittest.TextTestRunner(verbosity = 1).run(suite)

