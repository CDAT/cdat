#!/usr/bin/env python

# Test curvilinear grids

print 'Test 14: Generic grids ...',

import cdms, Numeric, MA, os, sys
from markError import clearError,markError,reportError
clearError()

datb = Numeric.array([ 693., 694.,])
latb = Numeric.array([-26.67690036,-30.99890917,])
lonb = Numeric.array([92.41822415, 94.4512163 ,])

f = cdms.open(os.path.join(sys.prefix,'sample_data','sampleGenGrid3.nc'))

# Slice a file variable on a curvilinear grid: by coordinates ...
samp = f['sample']
x = samp(lat=(-32,-25), lon=(90,95))
if not MA.allequal(x.raw_data(), datb):
    markError('slicing a file variable by coordinates')

grid = x.getGrid()
if grid.shape!=(2,):
    markError('grid shape is wrong')
lat = grid.getLatitude()
if not MA.allclose(lat.raw_data(), latb, atol=1.e-5):
    markError('latitude array is wrong')
lon = grid.getLongitude()
if not MA.allclose(lon.raw_data(), lonb, atol=1.e-5):
    markError('longitude array is wrong')

# ... and by index
y = samp[693:695]
if not MA.allequal(y, datb):
    markError('slicing a file variable by index')
grid = y.getGrid()
if not (grid.shape==(2,)):
    markError('index slice: grid shape is wrong')
lat = grid.getLatitude()
if not MA.allclose(lat.raw_data(), latb, atol=1.e-5):
    markError('index slice: latitude array is wrong')
lon = grid.getLongitude()
if not MA.allclose(lon.raw_data(), lonb, atol=1.e-5):
    markError('index slice: longitude array is wrong')

#-------------------------------------------------------------

# Slice a TRANSIENT variable on a curvilinear grid: by coordinates ...

samp = f['sample']
x = samp(lat=(-32,-25), lon=(90,95))
if not MA.allequal(x.raw_data(), datb):
    markError('slicing a transient variable by coordinates')

grid = x.getGrid()
if grid.shape!=(2,):
    markError('transient variable grid shape is wrong')
lat = grid.getLatitude()
if not MA.allclose(lat.raw_data(), latb, atol=1.e-5):
    markError('transient variable latitude array is wrong')
lon = grid.getLongitude()
if not MA.allclose(lon.raw_data(), lonb, atol=1.e-5):
    markError('transient variable longitude array is wrong')

# ... and by index
y = samp[693:695]
if not MA.allequal(y, datb):
    markError('slicing a transient variable by index')
grid = y.getGrid()
if not (grid.shape==(2,)):
    markError('transient variable index slice: grid shape is wrong')
lat = grid.getLatitude()
if not MA.allclose(lat.raw_data(), latb, atol=1.e-5):
    markError('transient variable index slice: latitude array is wrong')
lon = grid.getLongitude()
if not MA.allclose(lon.raw_data(), lonb, atol=1.e-5):
    markError('transient variable index slice: longitude array is wrong')

#-------------------------------------------------------------

# Computing with variables, coordinate variables
x2 = (9./5.)*x + 32.
lat2 = x2.getLatitude()
if not MA.allclose(lat.raw_data(), latb, atol=1.e-5):
    markError('latitude array not preserved after computation')

#-------------------------------------------------------------

# Slice a coordinate variable, computation

latsamp = samp.getLatitude()
latx = latsamp(cell=(693,694))
if not MA.allclose(latx.raw_data(), latb, atol=1.e-5):
    markError('transient variable index slice: latitude array is wrong')
latx = latsamp[693:695]
if not MA.allclose(latx.raw_data(), latb, atol=1.e-5):
    markError('transient variable index slice: latitude array is wrong')
latrad = latsamp*Numeric.pi/180.0

f.close()

#-------------------------------------------------------------

f = cdms.open(os.path.join(sys.prefix,'sample_data','cdtest14.xml'))

# Slice a DATASET variable on a curvilinear grid: by coordinates ...
samp = f['sample']
x = samp(lat=(-32,-25), lon=(90,95))
if not MA.allequal(x.raw_data(), datb):
    markError('slicing a dataset variable by coordinates')

grid = x.getGrid()
if grid.shape!=(2,):
    markError('dataset variable grid shape is wrong')
lat = grid.getLatitude()
if not MA.allclose(lat.raw_data(), latb, atol=1.e-5):
    markError('dataset variable latitude array is wrong')
lon = grid.getLongitude()
if not MA.allclose(lon.raw_data(), lonb, atol=1.e-5):
    markError('dataset variable longitude array is wrong')

# ... and by index
y = samp[693:695]
if not MA.allequal(y, datb):
    markError('slicing a dataset variable by index')
grid = y.getGrid()
if not (grid.shape==(2,)):
    markError('dataset variable index slice: grid shape is wrong')
lat = grid.getLatitude()
if not MA.allclose(lat.raw_data(), latb, atol=1.e-5):
    markError('dataset variable index slice: latitude array is wrong')
lon = grid.getLongitude()
if not MA.allclose(lon.raw_data(), lonb, atol=1.e-5):
    markError('dataset variable index slice: longitude array is wrong')

f.close()

reportError()
