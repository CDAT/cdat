import cdms2
from distarray.mvCubeDecomp import CubeDecomp
import numpy
import unittest
from mpi4py import MPI
from matplotlib import pylab
from mpl_toolkits.basemap import Basemap
import sys

class TestDistTransientVar(unittest.TestCase):
    """
    Test distributed array
    """

    def setUp(self):
        pass

    def Xtest_test0(self):
        """
        Test size, rank etc.
        """
        da = cdms2.open(sys.prefix + '/sample_data/clt.nc', 'r')('clt')
        rk = da.getMPIRank()
        sz = da.getMPISize()
        # make the data rank dependent
        da[:] = rk
        # create the halo regions
        da.exposeHalo(ghostWidth = 2)
        # ellipsis for one side
        slce = da.getHaloEllipsis(side=(1, 0, 0))
        # access remote data
        chunk = da.fetchHaloData((rk-1)%sz, side=(1, 0, 0))
        # check
        diff = float('inf')
        if rk != 0:
            diff = abs(numpy.sum(da[slce] - 1 - chunk))
        else:
            diff = abs(numpy.sum(da[slce] + sz - 1 - chunk))
        success = False
        if diff < 1.e-10:
            success = True
        self.assertEqual(success, True)
        # clean up
        da.freeHalo()

    def Xtest_test1(self):
        """
        Test case with domain decomposition
        """
        f = cdms2.open(sys.prefix + '/sample_data/clt.nc', 'r')
        cltVar = f['clt']

        # global sizes
        nLat, nLon = cltVar.shape[1:]

        # local rank and number of procs
        rk = MPI.COMM_WORLD.Get_rank()
        sz = MPI.COMM_WORLD.Get_size()

        # compute domain decomposition
        npLat, npLon = getDomainDecomp(sz, (nLat, nLon))
        if rk == 0:
            print ''
            print 'lat x lon sizes        : %d x %d ' % (nLat, nLon)
            print 'lat x lon domain decomp: %d x %d ' % (npLat, npLon)

        # number of points in each local domain
        nlat, nlon = nLat/npLat, nLon/npLon
        ipLat = rk // npLon
        ipLon = rk % npLon

        # starting/ending indices for local domain
        iLatBeg, iLatEnd = ipLat * nlat, (ipLat + 1) * nlat
        iLonBeg, iLonEnd = ipLon * nlon, (ipLon + 1) * nlon

        # now read local domain data for time 0
        clt = cltVar[0, iLatBeg:iLatEnd, iLonBeg:iLonEnd]

        # make halo available to other procs
        numGhosts = 1
        clt.exposeHalo(ghostWidth = numGhosts)

        # find the procs to the north, east, south, and west
        noProc = min(ipLat + 1, npLat - 1)*npLon + ipLon + 0
        soProc = max(ipLat - 1, 0        )*npLon + ipLon + 0
        # warp around
        eaProc = (ipLat + 0)*npLon + (ipLon + 1) % npLon
        weProc = (ipLat + 0)*npLon + (ipLon - 1) % npLon

        # fetch the remote data
        noCltData = clt.fetchHaloData(noProc, side=(-1, 0))
        soCltData = clt.fetchHaloData(soProc, side=(+1, 0))
        eaCltData = clt.fetchHaloData(eaProc, side=(0, -1))
        weCltData = clt.fetchHaloData(weProc, side=(0, +1))

        # slabs in local index space
        noSlc = clt.getHaloEllipsis(side=(-1, 0))
        soSlc = clt.getHaloEllipsis(side=(+1, 0))
        eaSlc = clt.getHaloEllipsis(side=(0, -1))
        weSlc = clt.getHaloEllipsis(side=(0, +1))
        
        tol = 1.e-10
        success = True
        if True:
            # north
            if ipLat < npLat - 1:
                print 'ipLat < npLat - 1:', ipLat, npLat-1
                igLatBeg = (ipLat + 1)*nlat
                igLatEnd = igLatBeg + numGhosts
                igLonBeg = ipLon*nlon
                igLonEnd = igLonBeg + nlon
                globalCltData = cltVar[0, igLatBeg:igLatEnd, igLonBeg:igLonEnd]
                if abs(numpy.sum(globalCltData - noCltData)) > tol:
                    success = False
            # south
            if ipLat >= 1:
                print 'ipLat >= 1:', ipLat, npLat-1
                igLatBeg = (ipLat + 0)*nlat - numGhosts
                igLatEnd = igLatBeg + numGhosts
                igLonBeg = ipLon*nlon
                igLonEnd = igLonBeg + nlon
                globalCltData = cltVar[0, igLatBeg:igLatEnd, igLonBeg:igLonEnd]
                if abs(numpy.sum(globalCltData - soCltData)) > tol:
                    success = False            
            # east
            igLatBeg = ipLat*nlat
            igLatEnd = igLatBeg + nlat
            igLonBeg = ((ipLon + 1)%npLon)*nlon
            igLonEnd = igLonBeg + numGhosts
            globalCltData = cltVar[0, igLatBeg:igLatEnd, igLonBeg:igLonEnd]
            if abs(numpy.sum(globalCltData - eaCltData)) > tol:
                success = False 
            # west
            igLatBeg = ipLat*nlat
            igLatEnd = igLatBeg + nlat
            igLonBeg = ((ipLon - 1)%npLon)*nlon + nlon - numGhosts
            igLonEnd = igLonBeg + numGhosts
            globalCltData = cltVar[0, igLatBeg:igLatEnd, igLonBeg:igLonEnd]
            if abs(numpy.sum(globalCltData - weCltData)) > tol:
                success = False 

        self.assertEqual(success, True)
        clt.freeHalo()

    def test_test2(self):
        """
        Apply the laplacian finite difference operator to clt
        """
        f = cdms2.open(sys.prefix + '/sample_data/clt.nc', 'r')
        cltVar = f['clt']

        # global sizes
        nLat, nLon = cltVar.shape[1:]

        # local rank and number of procs
        rk = MPI.COMM_WORLD.Get_rank()
        sz = MPI.COMM_WORLD.Get_size()

        # compute domain decomposition
        decomp = CubeDecomp(sz, (nLat, nLon))

        # number of processors along each axis
        npLat, npLon = None, None

        # list of slices
        slab = decomp.getSlab(rk)

        dc = decomp.getDecomp()
        if dc is not None:
            npLat, npLon = dc
            if rk == 0:
                print ''
                print 'lat x lon sizes        : %d x %d ' % (nLat, nLon)
                print 'lat x lon domain decomp: %d x %d ' % (npLat, npLon)
        else:
            if rk == 0:
                print 'No uniform decomp could be found for %d procs' % sz
            sys.exit(1)

        # starting/ending indices for local domain
        iLatBeg, iLatEnd = slab[0].start, slab[0].stop
        iLonBeg, iLonEnd = slab[1].start, slab[1].stop

        # now read local domain data for time 0
        clt = cltVar[0, iLatBeg:iLatEnd, iLonBeg:iLonEnd]

        # make halo available to other procs
        numGhosts = 1
        clt.exposeHalo(ghostWidth = numGhosts)

        # compute the star Laplacian in the interior, this does not require
        # any communication

        # laplacian = 4*clt[j, i] - clt[j+1, i] - clt[j-1, i] - clt[j, i+1] - clt[j, i-1]

        # data domain
        laplaceClt = 4 * clt[:]

        # local neighbor contributions, no communication
        laplaceClt[1:  , :] -= clt[0:-1,:]
        laplaceClt[0:-1, :] -= clt[1:  ,:]
        laplaceClt[:, 1:  ] -= clt[:,0:-1]
        laplaceClt[:, 0:-1] -= clt[:,1:  ]


        # now compute and fill in the halo

        # find the procs to the north, east, south, and west. This call will
        # return None if there is no neighbor. 
        noProc = decomp.getNeighborProc(rk, ( 1,  0), periodic = (False, True)) 
        soProc = decomp.getNeighborProc(rk, (-1,  0), periodic = (False, True)) 
        eaProc = decomp.getNeighborProc(rk, ( 0,  1), periodic = (False, True)) 
        weProc = decomp.getNeighborProc(rk, ( 0, -1), periodic = (False, True))

        # correct at north/south poles where zero flux condition applies
        if noProc is None: 
            laplaceClt[-1,:] -= clt[-1,:]
        if soProc is None:
            laplaceClt[0,:] -= clt[0,:]

        # fetch the remote data in the halo of the neighboring processor. When
        # the first argument is None, this amounts to a no-op (zero data are 
        # returned. Note that side refers to the neighbour domain. For instance,
        # the data to the west of the local domain correspond to the east halo
        # on the neighbouring processor.
        weCltData = clt.fetchHaloData(weProc, side=(0, +1))
        eaCltData = clt.fetchHaloData(eaProc, side=(0, -1))
        soCltData = clt.fetchHaloData(soProc, side=(+1, 0))
        noCltData = clt.fetchHaloData(noProc, side=(-1, 0))

        # finish the operator
        weSlc = clt.getHaloEllipsis(side=(0, -1))
        eaSlc = clt.getHaloEllipsis(side=(0, +1))
        soSlc = clt.getHaloEllipsis(side=(-1, 0))
        noSlc = clt.getHaloEllipsis(side=(+1, 0))

        laplaceClt[weSlc] -= weCltData
        laplaceClt[eaSlc] -= eaCltData
        if soProc is not None:
            laplaceClt[soSlc] -= soCltData
        if noProc is not None:
            laplaceClt[noSlc] -= noCltData

        if True:
            laplaceClt0 = numpy.zeros(cltVar.shape[1:], cltVar.dtype)
            # gather the data on proc 0
            laplaceClt0List = MPI.COMM_WORLD.gather(laplaceClt, root = 0)

            checksum = 0
            if rk == 0:
                for proc in range(sz):
                    slab = decomp.getSlab(proc)
                    iLatBeg, iLatEnd = slab[0].start, slab[0].stop
                    iLonBeg, iLonEnd = slab[1].start, slab[1].stop
                    laplaceClt0[iLatBeg:iLatEnd, iLonBeg:iLonEnd] = \
                        laplaceClt0List[proc]

                checksum = laplaceClt0.sum()
                print 'checksum = %20.15g' % checksum

                if False:
                    # plot
                    lat = cltVar.getLatitude()
                    lon = cltVar.getLongitude()
                    latmin, latmax = lat[:].min(), lat[:].max()
                    lonmin, lonmax = lon[:].min(), lon[:].max() 
                    mp = Basemap(llcrnrlat = latmin, urcrnrlat = latmax,
                                 llcrnrlon = lonmin, urcrnrlon = lonmax, 
                                 projection = 'cyl', resolution = 'l')
                    mp.pcolor(lon[:], lat[:], laplaceClt0)
                    mp.colorbar()
                    mp.drawcoastlines()
                    pylab.title('Laplacian of clt')
        
            # checks
            self.assertLess(abs(checksum), 1.e-3)

        # clean up
        clt.freeHalo()

        
if __name__ == '__main__':
    print "" # Spacer
    suite = unittest.TestLoader().loadTestsFromTestCase(TestDistTransientVar)
    unittest.TextTestRunner(verbosity = 1).run(suite)
    pylab.show()
    
