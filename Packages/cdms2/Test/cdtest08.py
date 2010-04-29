## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

#!/usr/bin/env python

import cdms2,numpy.ma, regrid2 as regrid, os, sys
from regrid2 import Regridder
from markError import clearError,markError,reportError
clearError()

print 'Test 8: Regridding ...',

## lat = cdms2.createGaussianAxis(32)
## lon = cdms2.createUniformLongitudeAxis(0.0,64,360.0/64.)
## outgrid = cdms2.createRectGrid(lat,lon,'yx','gaussian')
outgrid = cdms2.createGaussianGrid(32)

f = cdms2.openDataset(os.path.join(sys.prefix,'sample_data','readonly.nc'))
u = f.variables['u']
ingrid = u.getGrid()
try:
    sh = ingrid.shape
except:
    markError('Grid shape')

regridf = Regridder(ingrid, outgrid)
newu = regridf(u)

if (abs(newu[0,0,-1]-488.4763488) > 1.e-3): markError('regrid',newu[0,0,-1])
newu = u.regrid(outgrid)
if (abs(newu[0,0,-1]-488.4763488) > 1.e-3): markError('regrid',newu[0,0,-1])

# Regrid TV
tv = u.subSlice(0)
newtv = regridf(tv)
if (abs(newtv[0,0,-1]-488.4763488) > 1.e-3): markError('regrid tv',newtv[0,0,-1])
newtv = tv.regrid(outgrid)
if (abs(newtv[0,0,-1]-488.4763488) > 1.e-3): markError('regrid tv',newtv[0,0,-1])

# Regrid numpy.ma
ma = u[0]
newma = regridf(ma)
# Force slice result to be a scalar
if (abs(newma[0][-1]-488.4763488) > 1.e-3): markError('regrid ma',newma[0][-1])

# Regrid numpy
numar = numpy.ma.filled(u[0])
newar = regridf(numar)
if (abs(newar[0][-1]-488.4763488) > 1.e-3): markError('regrid numpy array',newar[0][-1])

# Regrid masked Variable
umasked = f.variables['umasked']
newum = regridf(umasked)
if (abs(newum[0,0,-1]-488.4763488) > 1.e-3): markError('regrid masked variable',newum[0,0,-1])

# Set explicit missing variable
numar = numpy.ma.filled(umasked[0])
newar = regridf(numar, missing=-99.9)
if (abs(newar[0][-1]-488.4763488) > 1.e-3): markError('regrid numpy array with missing flag',newar[0][-1])

# Set explicit mask
mask = umasked.subRegion().mask[0]
newar = regridf(numar, mask=mask)
if (abs(newar[0][-1]-488.4763488) > 1.e-3): markError('regrid numpy array with mask',newar[0][-1])

# Set the input grid mask
ingrid.setMask(mask)
regridf2 = Regridder(ingrid, outgrid)
newar = regridf2(numar)
if (abs(newar[0][-1]-488.4763488) > 1.e-3): markError('regrid numpy array with grid input mask',newar[0][-1])

# Dataset
g = cdms2.open(os.path.join(sys.prefix,'sample_data','test.xml'))
u = g.variables['u']
outgrid = cdms2.createGaussianGrid(24)
regridf3 = Regridder(u.getGrid(), outgrid)
try:
    unew = regridf3(u)
except:
    markError('regrid dataset variable')

lon2 = numpy.ma.array([  90.  , 101.25, 112.5 , 123.75, 135.  , 146.25, 157.5 , 168.75, 180.  ,
       191.25, 202.5 , 213.75, 225.  , 236.25, 247.5 , 258.75,])
lat2 = numpy.ma.array([-42.,-30.,-18., -6.,  6., 18., 30., 42.,])
grid2 = cdms2.createGenericGrid(lat2,lon2)
b1, b2 = grid2.getBounds()
grid2.setBounds(b1,b2)
latw,lonw = grid2.getWeights()

f.close()
# Test pressure regridder --------------------------------
import numpy
g = cdms2.createGaussianGrid(16)
levs = numpy.array([1.0,3.0,5.0])
lev = cdms2.createAxis(levs, id='level')
levsout = numpy.array([2.0,4.0])
levout = cdms2.createAxis(levsout, id='level')
dat = numpy.zeros((3,16,32), numpy.float32)
dat2 = numpy.zeros((2,16,32), numpy.float32)
dat[0] = 2.0
dat[1] = 4.0
dat[2] = 6.0
var = cdms2.createVariable(dat, axes=(lev,g), attributes={'units':'N/A'}, id='test')
result = var.pressureRegrid(levout)

if (abs(result[0,0,0]-3.26185) > 1.e-4): markError('regrid pressure',result[0,0,0])
# Test cross-section regridder --------------------------------
latin = cdms2.createGaussianAxis(16)
latout = cdms2.createGaussianAxis(24)
levsin = numpy.array([1.0,3.0,5.0])
lev = cdms2.createAxis(levsin, id='level')
levsout = numpy.array([2.0,4.0])
levout = cdms2.createAxis(levsout, id='level')
dat = numpy.zeros((3,16), numpy.float32)
dat[0] = 2.0
dat[1] = 4.0
dat[2] = 6.0
var = cdms2.createVariable(dat, axes=(lev,latin), attributes={'units':'N/A'}, id='test')
dat2 = var.crossSectionRegrid(levout, latout)
if (abs(dat2[0,0]-3.26185) > 1.e-4): markError('regrid cross-section',result[0,0,0])

reportError()
