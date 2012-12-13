"""
Testing CDAT regridding from curvilinear grid to rectangular grid
"""
import unittest
import cdms2
import ESMP
from cdms2.mvCdmsRegrid import CdmsRegrid
import numpy
import pylab
from genutil import minmax
from time import time
import operator
import sys

PLOT = False


class Test(unittest.TestCase):

    def setUp(self):
            pass

    def test0FixedByKindig(self):
        """
        Test from Kate Marvel
        As the following code snippet demonstrates, regridding a
        cdms2.tvariable.TransientVariable instance using regridTool='regrid2' 
        results in a new array that is masked everywhere.  regridTool='esmf' 
        and regridTool='libcf' both work as expected.

        Modified by Kindig to run.
        """
        import cdms2 as cdms
        import numpy as np

        filename = sys.prefix + '/sample_data/clt.nc'
        a=cdms.open(filename)
        data=a('clt')[0,...]

        print data.mask #verify this data is not masked

        ## Now create a new grid using every other latitude and every 
        #  other longitude
        LATS=data.getLatitude()[::2]
        lb = data.getLatitude().getBounds()[::2][:,0]
        ub = data.getLatitude().getBounds()[::2][:,1]
        latbounds = np.array(zip(lb, ub))

        self.assertEqual((ub != lb).all(), True)
        self.assertEqual(lb.min(), -90.0)

        LONS=data.getLongitude()[::2]
        lb=data.getLongitude().getBounds()[::2][:,0]
        ub=data.getLongitude().getBounds()[::2][:,1]
        lonbounds=np.array(zip(lb, ub))

        self.assertEqual((ub != lb).all(), True)
        self.assertEqual(lb.min(), -182.5)
        
        GRID=cdms.createGenericGrid(LATS,LONS,
                                    latBounds=latbounds,
                                    lonBounds=lonbounds)
        uniGrid = cdms.grid.createUniformGrid(-90, 23, 8, -180, 36, 10)

        test_gen = data.regrid(GRID,regridTool='regrid2')
        test_uni = data.regrid(uniGrid, regridTool = 'regrid2')
        

        # check that the mask does not extend everywhere...
        print data.mask, data.size
        
        print 'GENERIC', test_gen.mask.sum(), test_gen.size
        print 'UNIFORM', test_uni.mask.sum(), test_uni.size
        self.assertNotEqual(test_uni.mask.sum(), test_uni.size) 
        self.assertNotEqual(test_gen.mask.sum(), test_gen.size)
        
        if False:
            pylab.subplot(2, 1, 1)
            pylab.pcolor(data[...])
            pylab.title('data')
            pylab.subplot(2, 1, 2)
            pylab.pcolor(test_gen[...])
            pylab.title('test_gen (interpolated data)')
            pylab.show()

    def Xtest1(self):
        """
        Test from Kate Marvel
        As the following code snippet demonstrates, regridding a
        cdms2.tvariable.TransientVariable instance using regridTool='regrid2' 
        results in a new array that is masked everywhere.  regridTool='esmf' 
        and regridTool='libcf' both work as expected.

        This is similar to test0 but we only onterpolate over a single elevation,
        also fails. Likely GRID is not constructed correctly.
        """
        import cdms2 as cdms
        import numpy as np

        filename = sys.prefix + '/sample_data/clt.nc'
        a=cdms.open(filename)
        data=a('clt')[0,...]

        print data.mask #verify this data is not masked

        ## Now create a new grid using every other latitude and every 
        #  other longitude
        LATS=data.getLatitude()[::2]
        ub = data.getLatitude().getBounds()[1::2][:,0]
        lb = data.getLatitude().getBounds()[::2][:,1]
        latbounds = np.array(zip(ub,lb))

        self.assertEqual((ub != lb).all(), True)

        LONS=data.getLongitude()[::2]
        ub=data.getLongitude().getBounds()[1::2][:,0]
        lb=data.getLongitude().getBounds()[::2][:,1]
        lonbounds=np.array(zip(ub,lb))

        self.assertEqual((ub != lb).all(), True)
        

        GRID=cdms.createGenericGrid(LATS,LONS,
                                    latBounds=latbounds,
                                    lonBounds=lonbounds)

        test_data=data.regrid(GRID,regridTool='regrid2')

        # check that the mask does not extend everywhere...
        self.assertNotEqual(test_data.mask.sum(), test_data.size)
        
        if True:
            pylab.subplot(2, 1, 1)
            pylab.pcolor(data[...])
            pylab.title('data')
            pylab.subplot(2, 1, 2)
            pylab.pcolor(test_data[...])
            pylab.title('test_data (interpolated data)')
            pylab.show()

    def Xtest2(self):
        """
        Test from Kate Marvel
        As the following code snippet demonstrates, regridding a
        cdms2.tvariable.TransientVariable instance using regridTool='regrid2' 
        results in a new array that is masked everywhere.  regridTool='esmf' 
        and regridTool='libcf' both work as expected.

        This passes.
        """
        import cdms2 as cdms
        import numpy as np

        filename = sys.prefix + '/sample_data/clt.nc'
        a=cdms.open(filename)
        data=a('clt')[0,...]

        print data.mask #verify this data is not masked

        GRID= data.getGrid() # input = output grid, passes

        test_data=data.regrid(GRID,regridTool='regrid2')

        # check that the mask does not extend everywhere...
        self.assertNotEqual(test_data.mask.sum(), test_data.size)
        
        if False:
            pylab.subplot(2, 1, 1)
            pylab.pcolor(data[...])
            pylab.title('data')
            pylab.subplot(2, 1, 2)
            pylab.pcolor(test_data[...])
            pylab.title('test_data (interpolated data)')
            pylab.show()


    def Xtest3(self):
        """
        Test from Kate Marvel
        As the following code snippet demonstrates, regridding a
        cdms2.tvariable.TransientVariable instance using regridTool='regrid2' 
        results in a new array that is masked everywhere.  regridTool='esmf' 
        and regridTool='libcf' both work as expected.

        This is similar to the original test but we construct our own 
        uniform grid. This should passes.
        """
        import cdms2 as cdms
        import numpy as np

        filename = sys.prefix + '/sample_data/clt.nc'
        a=cdms.open(filename)
        data=a('clt')[0,...]

        print data.mask #verify this data is not masked

        GRID = cdms.grid.createUniformGrid(-90.0, 23, 8.0, -180.0, 36, 10.0, order="yx", mask=None)

        test_data=data.regrid(GRID,regridTool='regrid2')

        # check that the mask does not extend everywhere...
        self.assertNotEqual(test_data.mask.sum(), test_data.size)
        
        if False:
            pylab.subplot(2, 1, 1)
            pylab.pcolor(data[...])
            pylab.title('data')
            pylab.subplot(2, 1, 2)
            pylab.pcolor(test_data[...])
            pylab.title('test_data (interpolated data)')
            pylab.show()

    def test4DefaultRegridder(self):
        """
        use default regridder
        """
        import cdms2 as cdms
        import numpy as np

        filename = sys.prefix + '/sample_data/clt.nc'
        a=cdms.open(filename)
        data=a('clt')[0,...]

        print data.mask #verify this data is not masked

        ## Now create a new grid using every other latitude and every 
        #  other longitude
        LATS=data.getLatitude()[::2]
        lb = data.getLatitude().getBounds()[::2][:,0]
        ub = data.getLatitude().getBounds()[::2][:,1]
        latbounds = np.array(zip(lb, ub))

        self.assertEqual((ub != lb).all(), True)
        self.assertEqual(lb.min(), -90.0)

        LONS=data.getLongitude()[::2]
        lb=data.getLongitude().getBounds()[::2][:,0]
        ub=data.getLongitude().getBounds()[::2][:,1]
        lonbounds=np.array(zip(lb, ub))

        self.assertEqual((ub != lb).all(), True)
        self.assertEqual(lb.min(), -182.5)
        
        uniGrid = cdms.grid.createUniformGrid(-90, 23, 8, -180, 36, 10)
        test_uni = data.regrid(uniGrid)
        

        # check that the mask does not extend everywhere...
        print data.mask, data.size
        
        print 'UNIFORM', test_uni.mask.sum(), test_uni.size
        self.assertNotEqual(test_uni.mask.sum(), test_uni.size) 
        
        if PLOT:
            pylab.pcolor(data[...])
            pylab.title('data')
            pylab.savefig('testMarvel_test4DefaultRegridder.png')

if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    ESMP.ESMP_LogSet(True)
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()


