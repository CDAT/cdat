## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

#!/usr/bin/env python

# Test curvilinear grids

print 'Test 13: Curvilinear grids ...',

import cdms2, numpy, os, sys
from markError import clearError,markError,reportError
clearError()

datb = numpy.array(
    [[ 697., 698., 699., 700., 701., 702.,],
     [ 745., 746., 747., 748., 749., 750.,],
     [ 793., 794., 795., 796., 797., 798.,],
     [ 841., 842., 843., 844., 845., 846.,],
     [ 889., 890., 891., 892., 893., 894.,]])

latb = numpy.array(
    [[ -4.10403354, -4.10403354, -4.10403354, -4.10403354, -4.10403354, -4.10403354,],
     [  4.18758287,  4.18338422,  4.17731439,  4.1696268 ,  4.16062822,  4.150657  ,],
     [ 12.94714126, 12.9074324 , 12.85002385, 12.7773095 , 12.69218757, 12.59785563,],
     [ 22.13468161, 22.02120307, 21.85711809, 21.6492418 , 21.40582783, 21.1359872 ,],
     [ 31.57154392, 31.3483558 , 31.0255852 , 30.61656963, 30.13745176, 29.60604602,]])

lonb = numpy.array(
    [[  99.66846416, 109.38520711, 119.00985881, 128.50802363, 137.84762448,
        146.99940213,],
 [  99.66892374, 109.38595529, 119.01087021, 128.50926506, 137.84905702,
        147.00098336,],
 [  99.68231972, 109.40769163, 119.04011194, 128.54493672, 137.88992043,
        147.04571884,],
 [  99.74090681, 109.50235408, 119.16667376, 128.69809779, 138.06369873,
        147.23389768,],
 [  99.89478673, 109.74975596, 119.49504331, 129.09174691, 138.50528817,
        147.70588394,]])

maskb = numpy.array(
    [[0,0,0,0,0,0,],
     [0,0,0,0,0,0,],
     [0,0,0,0,0,0,],
     [0,0,0,0,0,0,],
     [1,1,1,1,1,0,]], 'b')

f = cdms2.open(os.path.join(sys.prefix,'sample_data','sampleCurveGrid4.nc'))

#-------------------------------------------------------------

# Slice a file variable on a curvilinear grid: by coordinates ...
samp = f['sample']
x=samp()
x = samp(lat=(-10,30), lon=(90,150))
if not numpy.ma.allequal(x.data, datb):
    markError('slicing a file variable by coordinates')
if not numpy.ma.allequal(x.mask, maskb):
    markError('slicing a file variable by coordinates: invalid mask')

grid = x.getGrid()
if not (grid.shape==(5,6)):
    markError('grid shape is wrong')
lat = grid.getLatitude()
if not numpy.ma.allclose(lat.data, latb, atol=1.e-5):
    markError('latitude array is wrong')
lon = grid.getLongitude()
if not numpy.ma.allclose(lon.data, lonb, atol=1.e-5):
    markError('longitude array is wrong')

# ... and by index
y = samp[14:19, 25:31]
if not numpy.ma.allequal(y, datb):
    markError('slicing a file variable by index')

grid = y.getGrid()
if not (grid.shape==(5,6)):
    markError('index slice: grid shape is wrong')
lat = grid.getLatitude()
if not numpy.ma.allclose(lat.data, latb, atol=1.e-5):
    markError('index slice: latitude array is wrong')
lon = grid.getLongitude()
if not numpy.ma.allclose(lon.data, lonb, atol=1.e-5):
    markError('index slice: longitude array is wrong')

#-------------------------------------------------------------

# Slice a TRANSIENT variable on a curvilinear grid: by coordinates ...
samp = f['sample']
x = samp(lat=(-10,30), lon=(90,150))
if not numpy.ma.allequal(x.data, datb):
    markError('slicing a transient variable by coordinates')
if not numpy.ma.allequal(x.mask, maskb):
    markError('slicing a transient variable by coordinates: invalid mask')

grid = x.getGrid()
if not (grid.shape==(5,6)):
    markError('transient variable grid shape is wrong')
lat = grid.getLatitude()
if not numpy.ma.allclose(lat.data, latb, atol=1.e-5):
    markError('transient variable latitude array is wrong')
lon = grid.getLongitude()
if not numpy.ma.allclose(lon.data, lonb, atol=1.e-5):
    markError('transient variable longitude array is wrong')

# ... and by index
y = samp[14:19, 25:31]
if not numpy.ma.allequal(y, datb):
    markError('transient variable: slicing a file variable by index')

grid = y.getGrid()
if not (grid.shape==(5,6)):
    markError('transient variable index slice: grid shape is wrong')
lat = grid.getLatitude()
if not numpy.ma.allclose(lat.data, latb, atol=1.e-5):
    markError('transient variable index slice: latitude array is wrong')
lon = grid.getLongitude()
if not numpy.ma.allclose(lon.data, lonb, atol=1.e-5):
    markError('transient variable index slice: longitude array is wrong')

#-------------------------------------------------------------

# Computing with variables, coordinate variables
x2 = (9./5.)*x + 32.
lat2 = x2.getLatitude()
if not numpy.ma.allclose(lat.data, latb, atol=1.e-5):
    markError('latitude array not preserved after computation')

#-------------------------------------------------------------

# Slice a coordinate variable, computation

latsamp = samp.getLatitude()
latx = latsamp(x=(25,30), y=(14,18))
if not numpy.ma.allclose(latx.data, latb, atol=1.e-5):
    markError('transient variable index slice: latitude array is wrong')
latx = latsamp[14:19,25:31]
if not numpy.ma.allclose(latx.data, latb, atol=1.e-5):
    markError('transient variable index slice: latitude array is wrong')
latrad = latsamp*numpy.pi/180.0

f.close()

#-------------------------------------------------------------

# Slice a DATASET variable on a curvilinear grid: by coordinates ...
f = cdms2.open(os.path.join(sys.prefix,'sample_data','cdtest13.xml'))

samp = f['sample']
x = samp(lat=(-10,30), lon=(90,150))
if not numpy.ma.allequal(x.data, datb):
    markError('slicing a dataset variable by coordinates')
if not numpy.ma.allequal(x.mask, maskb):
    markError('slicing a dataset variable by coordinates: invalid mask')

grid = x.getGrid()
if not (grid.shape==(5,6)):
    markError('dataset variable grid shape is wrong')
lat = grid.getLatitude()
if not numpy.ma.allclose(lat.data, latb, atol=1.e-5):
    markError('dataset variable latitude array is wrong')
lon = grid.getLongitude()
if not numpy.ma.allclose(lon.data, lonb, atol=1.e-5):
    markError('dataset variable longitude array is wrong')

# ... and by index
y = samp[14:19, 25:31]
if not numpy.ma.allequal(y, datb):
    markError('slicing a dataset variable by index')

grid = y.getGrid()
if not (grid.shape==(5,6)):
    markError('dataset variable index slice: grid shape is wrong')
lat = grid.getLatitude()
if not numpy.ma.allclose(lat.data, latb, atol=1.e-5):
    markError('dataset variable index slice: latitude array is wrong')
lon = grid.getLongitude()
if not numpy.ma.allclose(lon.data, lonb, atol=1.e-5):
    markError('dataset variable index slice: longitude array is wrong')

#-------------------------------------------------------------

# Test grid conversion
samp = f('sample')
curveGrid = samp.getGrid()
try:
    genGrid = curveGrid.toGenericGrid()
except:
    markError('converting to a generic grid')
f.close()

g = cdms2.open(os.path.join(sys.prefix,'sample_data','u_2000.nc'))
samp = g('u')
rectGrid = samp.getGrid()
try:
    curveGrid = rectGrid.toCurveGrid()
    genGrid = curveGrid.toGenericGrid()
except:
    markError('converting rectangular->curvilinear->generic grid')
reportError()
