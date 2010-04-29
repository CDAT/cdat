## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

#!/usr/bin/env python

import numpy
import cdms2, cdtime, copy, os, sys
from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

print 'Test 6: TransientVariables ...',

f = cdms2.open(os.path.join(sys.prefix,'sample_data','test.xml'))
v = f.variables['v']
vp = x[1,1:,4:12,8:25]
vp2 = vp[1,1:-1,1:]
tv = v.subRegion((366.,731.,'ccn'),(-42.,42.,'ccn'),(90.,270.))
tvv = v[0:2,0:10,30:40]
try:
    xx = tv[1,7,15]
except ValueError:
    markError("Scalar slice of transient variable")

# Variable get: axis, grid, latitude, level, longitude, missing, order, time, len, typecode

vaxis0 = v.getAxis(0)
axis0 = tv.getAxis(0)
if not numpy.ma.allequal(axis0[:],vaxis0[1:]): markError('getAxis: '+`axis0[:]`)

taxis = tv.getTime()
taxisarray = taxis[:]
vaxisarray = vaxis0[1:]
if not numpy.ma.allequal(taxisarray,vaxisarray): markError('getTime: '+`taxisarray`)

vaxis1 = v.getAxis(1)
lataxis = tv.getLatitude()
if not numpy.ma.allequal(lataxis[:],vaxis1[4:12]): markError('getLatitude: '+`lataxis[:]`)

vaxis2  = v.getAxis(2)
lonaxis = tv.getLongitude()

#
#  default is 'ccn' -- now it 8:25
#
if not numpy.ma.allequal(lonaxis[:],vaxis2[8:25]): markError('getLongitude: '+`lonaxis[:]`)

tv = v.subRegion((366.,731.,'ccn'),(-42.,42.,'ccn'),(90.,270.))
missing_value = v.getMissing()
if missing_value!=-99.9: markError('missing_value: '+`missing_value`)

tmv = tv.fill_value
if tmv!=-99.9: markError('TV missing_value', tmv)

grid = tv.getGrid()
if grid is None: markError('grid: '+`grid`)

order = tv.getOrder()
if order!='tyx': markError('order: '+order)

if len(tv)!=2: markError('length',len(tv))

# get TV domain
domain = tv.getDomain()
if len(domain)!=3: markError('domain', domain)

# getRegion of a TV
tv2 = tv.getRegion(731., (-30.,30.,'ccn'), (101.25, 270.0))
if not numpy.ma.allequal(tv2,vp2): markError('getRegion')

# Axis get: bounds, calendar, value, isXXX, len, subaxis, typecode
axis1 = tv.getAxis(1)
axis2 = tv.getAxis(2)
bounds = axis1.getBounds()
if bounds is None: markError('getBounds')
if axis0.getCalendar()!=cdtime.MixedCalendar: markError('getCalendar')
val = axis1.getValue()
if not numpy.ma.allequal(axis1.getValue(),axis1[:]): markError('getValue')
if not axis0.isTime(): markError('isTime')
if not axis1.isLatitude(): markError('isLatitude')
if not axis2.isLongitude(): markError('isLongitude')
#
# mf 20010405 if this PASSES it's an error
#
if axis2.isCircular(): markError('isCircular')
if len(axis2)!=17: markError('Axis length')

saxis = axis2.subAxis(1,-1)
if not numpy.ma.allequal(saxis[:],axis2[1:-1]): markError('subAxis',saxis[:])
if axis1.typecode()!=numpy.sctype2char(numpy.float): markError('Axis typecode')
if axis2.shape!=(17,): markError('Axis shape')

# Axis set: bounds, calendar
savebounds = copy.copy(bounds)
bounds[0,0]=-90.0
axis1.setBounds(bounds)
nbounds = axis1.getBounds()
if not numpy.ma.allequal(bounds,nbounds): markError('Axis setBounds')
axis0.setCalendar(cdtime.NoLeapCalendar)
if axis0.getCalendar()!=cdtime.NoLeapCalendar: markError('setCalendar')
gaussaxis = cdms2.createGaussianAxis(32)
try:
    testaxis = cdms2.createGaussianAxis(31)
except:
    markError('Gaussian axis with odd number of latitudes')

# Grid get: axis, bounds, latitude, longitude, mask, order, type, weights, subgrid, subgridRegion
a1 = grid.getAxis(1)
if not numpy.ma.allequal(a1[:],axis2[:]): markError('Grid getAxis')

bounds[0,0]=savebounds[0,0]
axis1.setBounds(bounds)
latbounds,lonbounds = grid.getBounds()
if not numpy.ma.allequal(latbounds,savebounds): markError('Grid getBounds')
glat = grid.getLatitude()
glon = grid.getLongitude()
mask = grid.getMask()
order = grid.getOrder()
if order!='yx': markError('Grid order',order)
gtype = grid.getType()
weights = grid.getWeights()
subg = grid.subGrid((1,7),(1,15))
subg2 = grid.subGridRegion((-30.,30.,'ccn'),(101.25,247.5,'ccn'))
if not numpy.ma.allequal(subg.getLongitude()[:], subg2.getLongitude()[:]): markError('subGrid')
if grid.shape!=(8,17): markError('Grid shape',grid.shape)

# Grid set: bounds, mask, type
latbounds[0,0]=-90.0
grid.setBounds(latbounds,lonbounds)
nlatb,nlonb = grid.getBounds()
if not numpy.ma.allequal(latbounds,nlatb): markError('Grid setBounds')
grid.setType('uniform')
if grid.getType()!='uniform': markError('Grid setType',grid.getType())

yy = numpy.ma.reshape(numpy.ma.arange(272.0),tv.shape)
tv.assignValue(yy)
if not numpy.ma.allequal(tv,yy): markError('TV assignValue')
tv3 = tv[0:-1]
if tv3.shape!=(1,8,17): markError('TV slice, negative index',tv3.shape)

# Create a transient variable from scratch
oldlat = tv.getLatitude()
oldBounds = oldlat.getBounds()
newlat = cdms2.createAxis(numpy.ma.array(oldlat[:]),numpy.ma.array(oldBounds))
b = newlat.getBounds()
b[0,0]=-48.
newlat.setBounds(b)

tv4 = cdms2.createVariable(tv[:],copy=1,fill_value=255.)
tv4[0,1:4]=20.0

if tv[:,::-1,:].shape != tv.shape: markError("Reversing axis direction")

# Test asVariable
www = cdms2.asVariable(tv4)
if www is not tv4: markError("asVariable failed, transient case.")
www = cdms2.asVariable (v, 0)
if www is not v:   markError("asVariable failed, transient case.")
www = cdms2.asVariable([1.,2.,3.])
if not cdms2.isVariable(www): markError("as/is test failed.")

# Check that createAxis allows an axis as an argument
lon = f.axes['longitude']
newlon = cdms2.createAxis(lon)
if newlon.typecode()=='O': markError("createAxis failed: allow axis arg")

# Test take of axis without bounds
newlat.setBounds(None)
samp = cdms2.axis.take(newlat,(2,4,6))

f.close()
reportError()
