#!/usr/bin/env python

import cdms,MA, Numeric, os, sys
from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

print 'Test 4: CdmsFile [MA] read/write ...',

time = MA.array([0.0,366.0,731.0])
lat = MA.arange(NLAT)*(180./(NLAT-1))-90.
lon = MA.arange(NLON)*(360.0/NLON)
timestr = ['2000','2001','2002']
u = x[0]

f = cdms.createDataset('readwrite.nc')
h = cdms.open(os.path.join(sys.prefix,'sample_data','readonly.nc'))
tobj = f.createAxis('time',MA.array([time[1]]))
tobj.units = 'days since 2000-1-1'
latobj = f.createAxis('latitude',lat)
latobj.units = 'degrees_north'
lonobj = f.createAxis('longitude',lon)
lonobj.units = 'degrees_east'
var = f.createVariable('u',MA.Float,(tobj,latobj,lonobj))
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

varattrs = var.attributes
var.long_name = 'Test variable'
var.param = -99
if var.param!=-99: markError("R/W param: "+var.param)
if (not varattrs.has_key('param')) or varattrs['param']!=-99: markError("Updating variable attributes")

fattrs = f.attributes
f.Conventions = 'CF1.0'
if f.Conventions!='CF1.0': markError("Cache global attribute")
if (not fattrs.has_key('Conventions')) or fattrs['Conventions']!='CF1.0': markError("Updating global attributes")

latattrs = latobj.attributes
latobj[NLAT/2] = 6.5
if latobj[NLAT/2]==lat[NLAT/2]: markError("Rewrite axis: %f"%vlat[NLAT/2])
lat[NLAT/2] = 6.5
latobj.standard_name = 'Latitude'
if latobj.standard_name!='Latitude': markError("Cache axis attribute")
if (not latattrs.has_key('standard_name')) or latattrs['standard_name']!='Latitude': markError("Updating axis attributes")


p0 = f.createVariable('p0',MA.Float,())
p0.assignValue(-99.9)

varmasked = f.createVariable('umasked',cdms.CdDouble,(tobj,latobj,lonobj),fill_value=-99.9)
umask = x[0,0]
umask.set_fill_value(-111.1)
umask[4:12,8:24] = MA.masked
fmask = MA.getmask(umask)
varmasked[:] = umask
varmasked.units = "m/s"
varmasked.long_name = "Eastward wind velocity"

uh = h.variables['u']
u2 = f.createVariableCopy(uh,'u2')
u2[:]=uh[:]

# Create an array with the special MA.masked value, write
x0 = x[0,0]
um2 = MA.where(MA.less(x0,128.0),MA.masked,x0)
f.write(um2, id='u3')

f.close()
#-----------------------------------------------------------
g = cdms.open('readwrite.nc','r+')
try:
    con = '<not read>'
    con = g.Conventions
except:
    markError("R/W global attr: "+con)
else:
    if g.Conventions!='CF1.0': markError("R/W global attr: "+con)

gdict = g.attributes
if not gdict.has_key('Conventions'): markError("File attribute dictionary",gdict)

var = g.variables['u']
vardict = var.attributes
if not vardict.has_key('long_name'): markError("Variable attribute dictionary")
try:
    ln = '<not read'
    ln = var.long_name
except:
    markError("R/W long_name: "+ln)
if ln!='Test variable': markError("long_name read",ln)

if var[0,4,0]!=-u[0,4,0]: markError("Read data: %f"%var[0,4,0])

vlat = var.getLatitude()
if vlat[NLAT/2]!=lat[NLAT/2]: markError("Read axis: %f"%vlat[NLAT/2])
if vlat.standard_name!='Latitude': markError("Read axis attribute: "+vlat.standard_name)
if vlat.units!='degrees_north': markError("Read axis units",vlat.units)

p0var = g.variables['p0']
val = p0var.getValue()
if val!=-99.9: markError("Read 0-D variable: %f"%val)

gvarm = g.variables['umasked']
gmaskm = gvarm.subSlice(squeeze=1)
if gmaskm.getMissing()!=-99.9: markError("Reading missing_value",gmaskm.getMissing())
mask = MA.getmask(gmaskm)
if mask is None or not MA.allequal(mask,fmask): markError("Reading mask",mask)

newlat = MA.array(vlat[:])
vlat.assignValue(newlat)

grid = gvarm.getGrid()
grid.setMask(mask)
m = grid.getMask()
if not MA.allequal(m,mask): markError("Get/set grid mask")

u3 = g.variables['u3']
lon = u3.getAxis(1)
lon.designateLongitude()
b = lon.getBounds()
if type(b)!=Numeric.ArrayType: markError("Bounds type is not Numeric")

g.close()
reportError()
