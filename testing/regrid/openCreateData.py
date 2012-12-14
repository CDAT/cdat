"""
$Id: openCreateData.py 2285 2012-06-24 18:01:49Z dkindig $


Plotting routine for tests in regrid2.ESMF using ginned up data
"""

# Allow imports from cdat_tests
import sys
sys.path.append('../cdat_tests')

import cdms2
import regrid2
import os
import numpy as np
import ESMP

home = os.getenv("HOME") + "/"
install_prefix = sys.prefix + '/'
sample_data_dir = sys.prefix + '/sample_data/' 

class dataNoPeri:
    def __init__(self, nx, ny, xCrdLimits, yCrdLimits):
        """
        Contruct a coordinate set for a non periodic grid using global 
        coordinates
        @param nx Number of x coordinate cell centers
        @param ny Number of y coordinate cell centers
        @param xBnds x nodal Bounds for the coordinates. Sum of xBnds must be 360
        @param yBnds y nodal Bounds for the coordinates
        """

        dims = [ny,nx]
        nx1, ny1 = nx+1, ny+1
        
        xLc, xUc = xCrdLimits[0], xCrdLimits[1]
        yLc, yUc = yCrdLimits[0], yCrdLimits[1]
        
        # Cell centered coordinates
        x = np.linspace(xLc, xUc, nx)
        y = np.linspace(yLc, yUc, ny)
        xx = np.outer(np.ones(ny,), x)
        yy = np.outer(y, np.ones(nx,))
        
        xInterval = abs(x[0]-x[1])/2.
        yInterval = abs(y[0]-y[1])/2.
        
        # Nodal coordinates
        xb = np.linspace(xLc-xInterval, xUc+xInterval, nx1)
        yb = np.linspace(yLc-yInterval, yUc+yInterval, ny1)
        xxb = np.outer(np.ones(ny1,), xb)
        yyb = np.outer(yb, np.ones(nx1,))

        # cell centered data
        d = 0 * xx; d[0, 0] = 1.0
        self.data = np.array(d, dtype = np.float32)
        self.dims = dims

        # Nodal data
        d = 0 * xxb; d[0,0] = 1.0
        self.dataN = np.array(d, dtype = np.float32)

        # Convert to a cdms2 variable
        flat = cdms2.axis.createAxis(y, id = 'lat')
        flat.units='degrees_north'
        flon = cdms2.axis.createAxis(x, id = 'lon')
        flon.units='degrees_east'
        self.cdmsFromCell = cdms2.grid.createRectGrid(flat, flon)
        self.cdmsFromCell.genBounds()

        self.cdmsFromData = cdms2.createVariable(self.data, mask = None, 
                                grid = self.cdmsFromCell,
                                axes = [flat, flon],
                                id = 'fromData')
        
        if yy.shape != xx.shape:
            print 'yy.shape', yy.shape, '!= xx.shape', xx.shape
            raise 'Coordinate shape mismatch'
        if yyb.shape != xxb.shape:
            print 'yyb.shape', yyb.shape, '!= xxb.shape', xxb.shape
            raise 'Bounds shape mismatch'
        self.coords = [xx, yy]
        self.bounds = [xxb, yyb]

class dataMaskedNoPeri:
    def __init__(self, nx, ny, xCrdLimits, yCrdLimits):
        """
        Contruct a coordinate set for a non periodic grid using global 
        coordinates
        @param nx Number of x coordinate cell centers
        @param ny Number of y coordinate cell centers
        @param xCrdLimits x nodal Bounds for the coordinates. Sum of xCrdLimits must be 360
        @param yCrdLimits y nodal Bounds for the coordinates
        """

        dims = [ny,nx]
        nx1, ny1 = nx+1, ny+1
        
        xLc, xUc = xCrdLimits[0], xCrdLimits[1]
        yLc, yUc = yCrdLimits[0], yCrdLimits[1]
        
        # Cell centered coordinates
        x = np.linspace(xLc, xUc, nx)
        y = np.linspace(yLc, yUc, ny)
        xx = np.outer(np.ones(ny,), x)
        yy = np.outer(y, np.ones(nx,))
        
        xInterval = abs(x[0]-x[1])/2.
        yInterval = abs(y[0]-y[1])/2.
        
        # Nodal coordinates
        xb = np.linspace(xLc-xInterval, xUc+xInterval, nx1)
        yb = np.linspace(yLc-yInterval, yUc+yInterval, ny1)
        xxb = np.outer(np.ones(ny1,), xb)
        yyb = np.outer(yb, np.ones(nx1,))

        # cell centered data
        d = 0 * xx; d[0, 0] = 1.0; d[1, 1] = 1.0
        mask = np.zeros(xx.shape, dtype = np.bool)
        self.data = np.ma.array(d, mask = mask, dtype = np.float32)
        self.data.mask[1, 1] = True
        self.dims = dims

        # Nodal data
        d = 0 * xxb; d[0, 0] = 1.0; d[1, 1] = 1.0
        mask = np.zeros(xxb.shape, dtype = np.bool)
        self.dataN = np.ma.array(d, mask = mask, dtype = np.float32)
        self.dataN.mask[1, 1] = True
        self.dims = dims

        # Convert to a cdms2 variable
        flat = cdms2.axis.createAxis(y, id = 'lat')
        flat.units='degrees_north'
        flon = cdms2.axis.createAxis(x, id = 'lon')
        flon.units='degrees_east'
        self.cdmsFromGrid = cdms2.grid.createRectGrid(flat, flon)
        self.cdmsFromGrid.genBounds()

        self.cdmsFromData = cdms2.createVariable(self.data, mask = None, 
                                grid = self.cdmsFromGrid,
                                axes = [flat, flon],
                                id = 'fromData')
        
        if yy.shape != xx.shape:
            print 'yy.shape', yy.shape, '!= xx.shape', xx.shape
            raise 'Coordinate shape mismatch'
        if yyb.shape != xxb.shape:
            print 'yyb.shape', yyb.shape, '!= xxb.shape', xxb.shape
            raise 'Bounds shape mismatch'
        self.coords = [yy, xx]
        self.bounds = [yyb, xxb]

class fakeData:
    def __init__(self, nx, ny, xBnds, yBnds, tnx, tny):
        """
        Contruct a coordinate set for a non periodic grid using global 
        coordinates
        @param nx Number of x coordinate cell centers
        @param ny Number of y coordinate cell centers
        @param xBnds x nodal Bounds for the coordinates. Sum of xBnds must be 360
        @param yBnds y nodal Bounds for the coordinates
        @param tnx Destination grid size. Use xBnds for limits
        @param tny Destination grid size. Use yBnds for limits
        """
        def getCrdAndBnd(start,stop,n):
            crd = np.linspace(start,stop,n)
            itv = abs(crd[0]-crd[1])/2
            bnd = np.linspace(start-itv,stop+itv,n+1)
            return crd, bnd

        fdims = np.array([nY, nX])
        nVals = fdims.prod()
        fLat,fLatB = getCrdAndBnd(lbY, ubY, nY)
        fLon,fLonB = getCrdAndBnd(lbX, ubX, nX)
        self.fCrd,dims = gsRegrid.makeCurvilinear([fLat, fLon])
        self.fBnd,dims = gsRegrid.makeCurvilinear([fLatB, fLonB])
        self.fakeFromGrid = [fLat, fLon]
        flat = cdms2.axis.createAxis(fLat, id = 'lat')
        flat.units='degrees_north'
        flon = cdms2.axis.createAxis(fLon, id = 'lon')
        flon.units='degrees_east'
        self.cdmsFromGrid = cdms2.grid.createRectGrid(flat, flon)
        nlat, nlon = len(fLat), len(fLon)
        
        tLat,tLatB = getCrdAndBnd(lbY, ubY, tnY)
        tLon,tLonB = getCrdAndBnd(lbX, ubX, tnX)

        self.tCrd,dims = gsRegrid.makeCurvilinear([tLat, tLon])
        self.tBnd,dims = gsRegrid.makeCurvilinear([tLatB, tLonB])

        self.fakeToGrid = self.tCrd
        tlat = cdms2.axis.createAxis(tLat, bounds=tLatB, id = 'lat')
        tlat.units='degrees_north'
        tlon = cdms2.axis.createAxis(tLon, bounds=tLonB, id = 'lon')
        tlon.units='degrees_east'
        self.cdmsToGrid = cdms2.grid.createRectGrid(tlat,tlon)
        
        fData = np.ma.zeros(fdims, np.float32) * 0.5
        fData[0,0] = 1
        mData = fData.copy()

        fData.mask = False
        self.fData = fData.copy()
        self.fData.id = 'fakeData'
        self.cdmsFromData = cdms2.createVariable(fData, mask = None, 
                                grid = self.cdmsFromGrid,
                                axes = [flat, flon],
                                id = 'fromData')
        self.tData = np.ma.zeros(self.tCrd[0].shape, self.fData.dtype)
        self.tData.mask = False
        nlat2,nlon2 = nlat/2,nlon/2
        pos1 = [nlat2-1,nlat2,nlat2+1]
        pos2 = [nlon2-1,nlon2,nlon2+1]
        pos1 = [1]
        pos2 = [2]

        # Initialize and populate the mask
        mData.mask = True
        mData.mask = False
        mData.mask[1,1] = True
#        for i in range(len(pos1)):
#            mData[pos1[i], :] = 1e20
#            mData[pos1[i], :].mask = True
#            mData[:, pos2[i]] = 1e20
#            mData[:, pos2[i]].mask = True
        self.mData = mData.copy()
        self.mData.id = 'maskData'

        self.fromMaskData = cdms2.createVariable(mData, 
                                grid = self.cdmsFromGrid,
                                axes = [flat, flon],
                                mask = mData.mask,
                                id = 'maskData')

        self.eps = 1e-7   # Epsilon
        # Point values to test against

        self.gclt = 180.0

class salinity:
    def __init__(self):
        filename = "so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc"
#        filename = "so_Omon_HadGEM2-CC_historical_r1i1p1_185912-186911_2timesteps.nc"
#        h=cdms2.open(sample_data_dir + '/' + filename)
        h=cdms2.open('./' + filename)
        data = h('so')[0, 0, ...]
        self.grid = [data.getLatitude(), data.getLongitude()]
        self.grid2D = self.grid
        self.data = data

class clt:
    def __init__(self):
        filename = "clt.nc"
        g=cdms2.open(sample_data_dir + '/' + filename)
        data = g('clt')
        self.grid = [data.getLatitude(), data.getLongitude()]
        g2D = data.getGrid().toCurveGrid()
        self.grid2D = [g2D.getLatitude()[:], g2D.getLongitude()[:]]
        self.data = data

class tas:
    def __init__(self):
        filename = "era40_tas_sample.nc"
        g=cdms2.open(sample_data_dir + '/' + filename)
        data = g('tas')
        self.grid = [data.getLatitude(), data.getLongitude()]
        g2D = data.getGrid().toCurveGrid()
        self.grid2D = [g2D.getLatitude()[:], g2D.getLongitude()[:]]
        self.data = data

