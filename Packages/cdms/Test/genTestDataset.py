#!/usr/bin/env python

# u(3,16,32), three separate files
# v(3,16,32), three separate files

import cdms, Numeric, os, MA
from MA import masked
NTIME = 3
NLAT = 16
NLON = 32

x = Numeric.arange(float(2*NTIME*NLAT*NLON))
x.shape=(2,NTIME,NLAT,NLON)
u = x[0]
v = x[1]
time = Numeric.array([0.0,366.0,731.0])
lat = Numeric.arange(NLAT)*(180./(NLAT-1))-90.
lon = Numeric.arange(NLON)*(360.0/NLON)
timestr = ['2000','2001','2002']

for id in ['u','v']:
    for i in range(len(timestr)):
        f = cdms.createDataset('%s_%s.nc'%(id,timestr[i]))
        tobj = f.createAxis('time',Numeric.array([time[i]]))
        tobj.units = 'days since 2000-1-1'
        latobj = f.createAxis('latitude',lat)
        latobj.units = 'degrees_north'
        lonobj = f.createAxis('longitude',lon)
        lonobj.units = 'degrees_east'
        var = f.createVariable(id,cdms.CdDouble,(tobj,latobj,lonobj))
        var.units = 'm/s'
        var.missing_value = -99.9
        if id=='u':
            var[:]=u[i]
        else:
            var[:]=v[i]
            
        f.Conventions = "CF-1.0"

        f.close()

try:
    os.chmod('readonly.nc',0644)
except:
    pass
f = cdms.createDataset('readonly.nc')
tobj = f.createAxis('time',Numeric.array([time[0]]))
tobj.units = 'days since 2000-1-1'
latobj = f.createAxis('latitude',lat)
latobj.units = 'degrees_north'
lonobj = f.createAxis('longitude',lon)
lonobj.units = 'degrees_east'
var = f.createVariable('u',cdms.CdDouble,(tobj,latobj,lonobj))
var.units = 'm/s'
var[:]=u[0]
mvar = f.createVariable('umasked',cdms.CdDouble,(tobj,latobj,lonobj))
umask = MA.array(u[0])
umask[1] = masked
mvar[:] = umask[:]
mvar.missing_value = umask.fill_value()
f.close()

os.chmod('readonly.nc',0444)
