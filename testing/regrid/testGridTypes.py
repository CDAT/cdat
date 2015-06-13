import cdat_info
import cdms2
import ESMP
import numpy
import unittest
import sys

class TestGridTypes(unittest.TestCase):
    """
    All test interpolate to the same grid or to curvilinear grid
    """

    def setUp(self):
        pass

    def test_test1(self):
        """
        2D gsRegrid
        """
        u = cdms2.open(cdat_info.get_sampledata_path() + '/clt.nc')('u')[0, 0,...]
        clt = cdms2.open(cdat_info.get_sampledata_path() + '/clt.nc')('clt')[0, ...]
        ctlOnUGrid = clt.regrid( u.getGrid() )
        #print 'ctlOnUGrid.getGrid() = ', type(ctlOnUGrid.getGrid())
        self.assertRegexpMatches(str(type(ctlOnUGrid.getGrid())),
                         "cdms2.grid.TransientRectGrid")

    def test_test2(self):
        """
        2D ESMP
        """
        u = cdms2.open(cdat_info.get_sampledata_path() + '/clt.nc')('u')[0, 0,...]
        clt = cdms2.open(cdat_info.get_sampledata_path() + '/clt.nc')('clt')[0, ...]
        ctlOnUGrid = clt.regrid( u.getGrid(), regridTool = "ESMP" )
        #print 'ctlOnUGrid.getGrid() = ', type(ctlOnUGrid.getGrid())
        self.assertRegexpMatches(str(type(ctlOnUGrid.getGrid())),
                         "cdms2.grid.TransientRectGrid")


if __name__ == '__main__':
    print "" # Spacer
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(TestGridTypes)
    unittest.TextTestRunner(verbosity = 1).run(suite)

