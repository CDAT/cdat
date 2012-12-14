"""
$Id: testUnstructGrid.py 2354 2012-07-11 15:28:14Z pletzer $

Unit tests for unstructured grid

"""

import operator
import numpy
import cdms2
import unittest
from regrid2 import esmf
import ESMP


class Test(unittest.TestCase):

    def setUp(self):
        pass

    def test_2d_esmf_simple(self):
        """
        0-1
        | |\
        2-3-4
        """
        ntopo = 2
        nspace = 2
        grd = esmf.EsmfUnstructGrid(ntopo, nspace)
        inds = numpy.array([0, 1, 2, 3, 4], numpy.int32)
        coords = numpy.array([0., 1., 
                              1., 1., 
                              0., 0., 
                              1., 0., 
                              2., 0.],
                             numpy.float64)
        grd.setNodes(inds, coords)
        cellInds = numpy.array([0, 1], numpy.int32)
        cellTypes = numpy.array([ESMP.ESMP_MESHELEMTYPE_QUAD, ESMP.ESMP_MESHELEMTYPE_TRI], numpy.int32)
        conn = numpy.array([2, 3, 1, 0, 
                            3, 4, 1], numpy.int32)
        grd.setCells(cellInds, cellTypes, conn)
        grd.toVTK('test_2d_simple.vtk')
        

if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)


