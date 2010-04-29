## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

#!/usr/bin/env python

import numpy, cdms2, os, cdtime, sys
from markError import clearError,markError,reportError
clearError()

print 'Test 10: Dataset (filemap) ...',

NYR = 6
NMO = 12
NLAT = 16
NLON = 32
timear = numpy.ma.arange(NYR*NMO, dtype=numpy.float)
time = cdms2.createAxis(timear, id='time')
time.units = "months since 2000-1"
g = cdms2.createUniformGrid(-90.0, NLAT, 180./(NLAT-1), 0., NLON, 360./NLON)
uar = numpy.ma.arange(NYR*NMO*NLAT*NLON)
uar.shape = (NYR*NMO,NLAT,NLON)
u = cdms2.createVariable(uar, id='u', axes = (time, g.getLatitude(), g.getLongitude()))
u.units = 'm/s'

var = uar + 100000.
v = cdms2.createVariable(var, id='v', axes = u.getAxisList())
v.units = 'm/s'

tar = uar + 200000.
t  = cdms2.createVariable(tar, id='t', axes = u.getAxisList())
t.units = 'K'

imo = 0
for year in range(2000,2006):
    f = cdms2.open('cdtest10_uv_%d.nc'%year,'w')
    uchunk = u.subSlice((imo,imo+12))
    f.write(uchunk)
    vchunk = v.subSlice((imo,imo+12))
    f.write(vchunk)
    f.close()
    g = cdms2.open('cdtest10_t_%d.nc'%year,'w')
    if year==2004:
        tchunk = t.subSlice(imo+6)
    else:
        tchunk = t.subSlice((imo,imo+12))
    g.write(tchunk)
    g.close()
    imo += 12

os.unlink('cdtest10_t_2001.nc')
os.unlink('cdtest10_t_2003.nc')
os.unlink('cdtest10_uv_2003.nc')
os.unlink('cdtest10_uv_2005.nc')

# Test filemap access ...
try:
    f = cdms2.open('cdtest10.xml')
except Exception,err:
    print err
    markError('Opening cdtest10.xml using filemap: Version 3.0 or greater required')
    reportError()
    sys.exit(1)

# Read u from 2001-7 to 2002-7 and compare
u = f.getVariable('u')
t = u.getTime()
t1 = cdtime.comptime(2001,7).torel(t.units).value
t2 = cdtime.comptime(2002,7).torel(t.units).value
uar2 = u.getRegion(time=(t1,t2,'con'))
uar2p = uar[18:30]
if not numpy.ma.allclose(uar2,uar2p): markError('u.getRegion() from 2001-7 to 2002-7 failed',uar2[0,0,0])

# Read u from 2003-7 to 2004-7 and compare
t1 = cdtime.comptime(2003,7).torel(t.units).value
t2 = cdtime.comptime(2004,7).torel(t.units).value
uar3 = u.getRegion(time=(t1,t2,'con'))
uar3p = uar[48:54]
if not numpy.ma.allclose(uar3,uar3p): markError('u.getRegion() from 2003-7 to 2004-7 failed',uar3[0,0,0])

# Read t from 2003-7 to 2005-7 and compare
tt = f.getVariable('t')
t1 = cdtime.comptime(2003,7).torel(t.units).value
t2 = cdtime.comptime(2005,7).torel(t.units).value
tar2 = tt.getRegion(time=(t1,t2,'con'))
tar2p = numpy.ma.concatenate((tar[numpy.newaxis,54],tar[60:66]))
if not numpy.ma.allclose(tar2,tar2p): markError('t.getRegion() from 2003-7 to 2005-7 failed',tar2[0,0,0])

os.unlink('cdtest10_t_2000.nc')
os.unlink('cdtest10_t_2002.nc')
os.unlink('cdtest10_t_2004.nc')
os.unlink('cdtest10_t_2005.nc')
os.unlink('cdtest10_uv_2000.nc')
os.unlink('cdtest10_uv_2001.nc')
os.unlink('cdtest10_uv_2002.nc')
os.unlink('cdtest10_uv_2004.nc')

f.close()
reportError()
