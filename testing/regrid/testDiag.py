"""
$Id: testDiag.py 2389 2012-07-26 15:51:43Z dkindig $

Test diagnostics

"""

import re
import numpy
import cdms2
import regrid2
import unittest
import ESMP
from regrid2 import esmf
from matplotlib import pylab
from mpi4py import MPI
import types
from math import pi
import sys


class Test(unittest.TestCase):

    def setUp(self):
        self.pe = MPI.COMM_WORLD.Get_rank()
        self.nprocs = MPI.COMM_WORLD.Get_size()
 
    def Xtest1_libcf(self):
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')
        diag = {'numValid': None, 'numDstPoints': None}
        soInterp = so.regrid(clt.getGrid(), 
                             regridTool = 'libcf', 
                             regridMethod='linear', 
                             diag = diag)
        if self.pe == 0:
            # diag =  {'numDstPoints': 3312, 'numValid': 2933}
            self.assertEqual(diag['numDstPoints'], 3312)
            self.assertEqual(diag['numValid'], 2933)
        if False:
            pylab.subplot(1, 2, 1)
            pylab.pcolor(so, vmin = 20, vmax = 40)
            pylab.colorbar()
            pylab.title('so')
            pylab.subplot(1, 2, 2)
            pylab.pcolor(soInterp, vmin = 20, vmax = 40)
            pylab.colorbar()
            pylab.title('soInterp')

    def test2_varRegrid(self):
        print
        print 'test2_varRegrid'
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')
        diag = {'srcAreas': None, 'dstAreas': None,
                'srcAreaFractions': None, 'dstAreaFractions': None}
        soInterp = so.regrid(clt.getGrid(), 
                             regridTool = 'esmf', 
                             regridMethod='conserve', 
                       	     diag = diag)
        if self.pe == 0:
            totSrcArea = diag['srcAreas'].sum()
            totDstArea = diag['dstAreas'].sum()
            totSrcFrac = diag['srcAreaFractions'].sum()
            self.assertEqual(numpy.isnan(totSrcFrac).sum(), 0)
            self.assertLess(abs(totSrcArea - 4*pi)/(4*pi), 0.02)
            self.assertLess(abs(totDstArea - 4*pi)/(4*pi), 0.01)
            soMass = (so*diag['srcAreas']).sum()
            inMass = (soInterp*diag['dstAreas']).sum()
            print soMass, inMass
            diff = abs(soMass - inMass)/soMass
            self.assertLess(diff, 7.e-7)
        if False:
            pylab.subplot(1, 2, 1)
            pylab.pcolor(so, vmin = 20, vmax = 40)
            pylab.colorbar()
            pylab.title('so')
            pylab.subplot(1, 2, 2)
            pylab.pcolor(soInterp, vmin = 20, vmax = 40)
            pylab.colorbar()
            pylab.title('soInterp')
    
    def Xtest3_esmf(self):
        print
        print 'test3_esmf'
        srcF = cdms2.open(sys.prefix + \
                              '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
        so = srcF('so')[0, 0, ...]
        clt = cdms2.open(sys.prefix + '/sample_data/clt.nc')('clt')[0, ...]
        diag = {'srcAreas': None, 'dstAreas': None,
                'srcAreaFractions': None, 'dstAreaFractions': None}

        srcCoords = [so.getGrid().getLatitude()[:], so.getGrid().getLongitude()[:]]
        srcBounds = cdms2.mvCdmsRegrid.getBoundList(srcCoords)
        tmp = clt.getGrid().toCurveGrid()
        dstCoords = [tmp.getLatitude()[:], tmp.getLongitude()[:]]
        dstBounds = cdms2.mvCdmsRegrid.getBoundList(dstCoords)

        # Establish the grids
        srcGrid = esmf.EsmfStructGrid(so.shape, periodicity = 1)
        dstGrid = esmf.EsmfStructGrid(clt.shape, periodicity = 1)

        srcGrid.setCoords(srcCoords)
        dstGrid.setCoords(dstCoords)
        srcGrid.setCoords(srcBounds, staggerloc = esmf.CORNER)
        dstGrid.setCoords(dstBounds, staggerloc = esmf.CORNER)

        # Establish the fields
        srcFeld = esmf.EsmfStructField(srcGrid, 'srcFeld', so.dtype)
        dstFeld = esmf.EsmfStructField(dstGrid, 'dstFeld',so.dtype)

        srcFeldPtr = srcFeld.getPointer()
        srcFeldPtr[:] = so.data.flat
        dstFeldPtr = dstFeld.getPointer()
        dstFeldPtr[:] = so.missing_value

        # Fractions
        srcFrac = esmf.EsmfStructField(srcGrid, 'srcFrac', so.dtype)
        dstFrac = esmf.EsmfStructField(dstGrid, 'dstFrac', so.dtype)

        srcFracPtr = srcFrac.getPointer()
        srcFracPtr[:] = 1.0
        dstFracPtr = dstFrac.getPointer()
        dstFracPtr[:] = 1.0

        nnan = numpy.isnan(srcFeldPtr).sum()
        if nnan > 0: print "There are nan's in srcFracPtr"

        # Areas
        presrcArea = esmf.EsmfStructField(srcGrid, 'srcArea', so.dtype)
        predstArea = esmf.EsmfStructField(dstGrid, 'dstArea', so.dtype)

        presrcAreaPtr = presrcArea.getPointer()
        presrcAreaPtr[:] = 1.0
        predstAreaPtr = predstArea.getPointer()
        predstAreaPtr[:] = 1.0

        ro = esmf.EsmfRegrid(srcFeld, dstFeld, 
                             srcFrac = srcFrac, dstFrac = dstFrac,
                             srcMaskValues = None, dstMaskValues = None,
                             regridMethod = esmf.CONSERVE, 
                             unMappedAction = esmf.IGNORE)
        ro()

        srcAreas = ro.getSrcAreas(None)
        dstAreas = ro.getDstAreas(None)

        srcFracPtrPost = ro.getSrcAreaFractions(None)
        nnanPost = numpy.isnan(srcFracPtrPost).sum()
        nnan = numpy.isnan(srcFracPtr).sum()
        if nnan > 0 or nnanPost > 0: 
            print "There are nan's in srcFracPtrPost", nnanPost
            print "There are nan's in srcFracPtr", nnan
        self.assertNotEqual(nnan, 0)
        self.assertNotEqual(nnanPost, 0)

        dstFracPtrPost = ro.getDstAreaFractions(None)
        nnanPost = numpy.isnan(dstFracPtrPost).sum()
        nnan = numpy.isnan(dstFracPtrPost).sum()
        if nnan > 0 or nnanPost > 0: 
            print "There are nan's in dstFracPtrPost", nnanPost
            print "There are nan's in dstFracPtr", nnan
        self.assertNotEqual(nnan, 0)
        self.assertNotEqual(nnanPost, 0)

        srcMass = (srcFeldPtr * srcAreas.flatten() * srcFracPtr).sum()

        dstMass = (dstFeldPtr * dstAreas.flatten()).sum()
        dstMassPtr = (dstFeldPtr * predstAreaPtr).sum()

        diff = abs(srcMass - dstMass)
        self.assertLess(diff/srcMass, 1.e-7)

if __name__ == '__main__':
    print ""
    ESMP.ESMP_Initialize()
    suite = unittest.TestLoader().loadTestsFromTestCase(Test)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()


