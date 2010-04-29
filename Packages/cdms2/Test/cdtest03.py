## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

#!/usr/bin/env python

import cdms2,numpy
from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

print 'Test 3: CdmsFile [numpys] read/write ...',

time = numpy.array([0.0,366.0,731.0])
lat = numpy.arange(NLAT)*(180./(NLAT-1))-90.
lon = numpy.arange(NLON)*(360.0/NLON)
timestr = ['2000','2001','2002']
u = x[0]

f = cdms2.createDataset('readwrite.nc')
tobj = f.createAxis('time',numpy.array([time[1]]))
tobj.units = 'days since 2000-1-1'
latobj = f.createAxis('latitude',lat)
latobj.units = 'degrees_north'
lonobj = f.createAxis('longitude',lon)
lonobj.units = 'degrees_east'
var = f.createVariable('u',cdms2.CdDouble,(tobj,latobj,lonobj))
var.units = 'm/s'
try:
    var[:]=u[0]
except:
    markError("Setting a slice")
try:
    var.assignValue(u[0])
except:
    markError("Assigning an array")
try:
    var[0,4:12] = -u[0,4:12]
except:
    markError("Setting an item")

var.long_name = 'Test variable'
var.param = -99
if var.param!=-99: markError("R/W param: "+var.param)

f.Conventions = 'CF1.0'

latobj[NLAT/2] = 6.5
if latobj[NLAT/2]==lat[NLAT/2]: markError("Rewrite axis: %f"%vlat[NLAT/2])
lat[NLAT/2] = 6.5
latobj.standard_name = 'Latitude'

p0 = f.createVariable('p0',cdms2.CdDouble,())
p0.assignValue(-99.9)

f.close()
#-----------------------------------------------------------
g = cdms2.openDataset('readwrite.nc','r+')
con = g.Conventions
try:
    con = '<not read>'
    con = g.Conventions
except:
    markError("R/W global attr: "+con)
else:
    if g.Conventions!='CF1.0': markError("R/W global attr: "+con)

var = g.variables['u']
try:
    ln = '<not read'
    ln = var.long_name
except:
    markError("R/W long_name: "+ln)

if var[0,4,0]!=-u[0,4,0]: markError("Read data: %f"%var[0,4,0])

vlat = var.getLatitude()
if vlat[NLAT/2]!=lat[NLAT/2]: markError("Read axis: %f"%vlat[NLAT/2])
if vlat.standard_name!='Latitude': markError("Read axis attribute: "+vlat.standard_name)

p0var = g.variables['p0']
val = p0var.getValue()
if val!=-99.9: markError("Read 0-D variable: %f"%val)

newlat = vlat[:]
vlat.assignValue(newlat)

g.close()
reportError()
