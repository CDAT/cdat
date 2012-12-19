"""
$Id: testSalinity.py 2277 2012-06-22 23:30:40Z dkindig $

Test interpolation on salinity datasets

"""

import operator
import numpy
import cdms2
import regrid2
import unittest
import time
import copy
from matplotlib import pylab
import sys

PLOT = False

class Test(unittest.TestCase):

    def setUp(self):
        pass

    def Xtest1_gsRegrid(self):
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')
        srcGrd = [so.getGrid().getLatitude(), so.getGrid().getLongitude()]
        dstGrd = [clt.getGrid().getLatitude(), clt.getGrid().getLongitude()]
        # create regrid object
        r = regrid2.gsRegrid.Regrid(srcGrd, dstGrd, src_bounds=None, 
                                    mkCyclic=False, handleCut=False)
        # set mask
        r.setMask( (so == so.missing_value) )
        # compute interpolation weights
        r.computeWeights(nitermax=20, tolpos=0.01)
        # make sure most of the weights have ben found
        print 'ratio of valid over number of nodes: %f' % (r.getNumValid()/float(r.getNumDstPoints()))
        self.assertGreater(r.getNumValid(), 0)
        # create dst data container
        dstShp = r.getDstGrid()[0].shape
        print 'dst data shape: ', dstShp
        dstData = numpy.ones(dstShp, so.dtype) * so.missing_value
        # interpolate
        r.apply(so, dstData)
        # checks
        dstDataMask = (dstData == so.missing_value)
        dstDataFltd = dstData * (1 - dstDataMask)
        if so.missing_value > 0:
            dstDataMin = dstData.min()
            dstDataMax = dstDataFltd.max()
        else:
            dstDataMin = dstDataFltd.min()
            dstDataMax = dstData.max()
        print 'min/max value of dstData: %f %f' % (dstDataMin, dstDataMax)                               
        self.assertGreater(dstDataMin, so.min())
        self.assertLess(dstDataMax, so.max())

    def Xtest2_libCFRegrid(self):
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')
        srcGrd = [so.getGrid().getLatitude(), so.getGrid().getLongitude()]
        dstGrd = [clt.getGrid().getLatitude(), clt.getGrid().getLongitude()]
        # create regrid object
        r = regrid2.mvLibCFRegrid.LibCFRegrid(srcGrd, dstGrd, 
                                              srcGridMask=(so == so.missing_value), 
                                              srcBounds=None, mkCyclic=False, 
                                              handleCut=False)
        # compute weights
        r.computeWeights(nitermax=20, tolpos=0.01)
        # create dst data container
        dstShp = r.getDstGrid()[0].shape
        dstData = numpy.ones(dstShp, so.dtype) * so.missing_value
        # interpolate
        r.apply(so, dstData)
        # checks
        dstDataMask = (dstData == so.missing_value)
        dstDataFltd = dstData * (1 - dstDataMask)
        if so.missing_value > 0:
            dstDataMin = dstData.min()
            dstDataMax = dstDataFltd.max()
        else:
            dstDataMin = dstDataFltd.min()
            dstDataMax = dstData.max()
        print 'min/max value of dstData: %f %f' % (dstDataMin, dstDataMax)                               
        self.assertGreater(dstDataMin, so.min())
        self.assertLess(dstDataMax, so.max())
        

    def Xtest3_genericRegrid(self):
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')
        srcGrd = [so.getGrid().getLatitude(), so.getGrid().getLongitude()]
        dstGrd = [clt.getGrid().getLatitude(), clt.getGrid().getLongitude()]
        # create regrid object
        r = regrid2.mvGenericRegrid.GenericRegrid(srcGrd, dstGrd, so.dtype,
                                                  regridMethod='linear', regridTool='libcf',
                                                  srcGridMask=(so == so.missing_value), srcBounds=None, srcGridAreas=None,
                                                  dstGridMask=None, dstBounds=None, dstGridAreas=None,
                                                  mkCyclic=False, handleCut=False)
        # compute weights
        r.computeWeights(nitermax=20, tolpos=0.01)
        # create dst data container
        dstShp = r.getDstGrid()[0].shape
        dstData = numpy.ones(dstShp, so.dtype) * so.missing_value
        # interpolate
        r.apply(so, dstData)
        # checks
        dstDataMask = (dstData == so.missing_value)
        dstDataFltd = dstData * (1 - dstDataMask)
        if so.missing_value > 0:
            dstDataMin = dstData.min()
            dstDataMax = dstDataFltd.max()
        else:
            dstDataMin = dstDataFltd.min()
            dstDataMax = dstData.max()
        print 'min/max value of dstData: %f %f' % (dstDataMin, dstDataMax)                               
        self.assertGreater(dstDataMin, so.min())
        self.assertLess(dstDataMax, so.max())

    def test4_cdmsRegrid(self):
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')
        # create regrid object
        r = cdms2.CdmsRegrid(so.getGrid(), clt.getGrid(), so.dtype,
                             regridMethod='linear', regridTool='libcf',
                             srcGridMask=(so == so.missing_value), 
                             srcGridAreas=None,
                             dstGridMask=None, dstGridAreas=None,
                             mkCyclic=False, handleCut=False)
        dstData = r(so)
        # checks
        dstDataMask = (dstData == so.missing_value)
        dstDataFltd = dstData * (1 - dstDataMask)
        if so.missing_value > 0:
            dstDataMin = dstData.min()
            dstDataMax = dstDataFltd.max()
        else:
            dstDataMin = dstDataFltd.min()
            dstDataMax = dstData.max()
        print 'min/max value of dstData: %f %f' % (dstDataMin, dstDataMax)                               
        self.assertGreater(dstDataMin, so.min())
        self.assertLess(dstDataMax, so.max())


    def test5_regrid(self):
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')
        dstData = so.regrid(clt.getGrid())
        dstDataMask = (dstData == so.missing_value)
        dstDataFltd = dstData * (1 - dstDataMask)
        if so.missing_value > 0:
            dstDataMin = dstData.min()
            dstDataMax = dstDataFltd.max()
        else:
            dstDataMin = dstDataFltd.min()
            dstDataMax = dstData.max()
        print 'min/max value of dstData: %f %f' % (dstDataMin, dstDataMax)                               
        self.assertGreater(dstDataMin, so.min())
        self.assertLess(dstDataMax, so.max())

if __name__ == '__main__':
    print ""
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()

