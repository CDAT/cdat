## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

#!/usr/bin/env python

# u(3,16,32), three separate files
# v(3,16,32), three separate files

import cdms2, numpy, os
NTIME = 3
NLAT = 16
NLON = 32

x = numpy.arange(float(2*NTIME*NLAT*NLON))
x.shape=(2,NTIME,NLAT,NLON)
u = x[0]
v = x[1]
time = numpy.array([0.0,366.0,731.0])
lat = numpy.arange(NLAT)*(180./(NLAT-1))-90.
lon = numpy.arange(NLON)*(360.0/NLON)
timestr = ['2000','2001','2002']

for id in ['u','v']:
    for i in range(len(timestr)):
        f = cdms2.createDataset('%s_%s.nc'%(id,timestr[i]))
        tobj = f.createAxis('time',numpy.array([time[i]]))
        tobj.units = 'days since 2000-1-1'
        latobj = f.createAxis('latitude',lat)
        latobj.units = 'degrees_north'
        lonobj = f.createAxis('longitude',lon)
        lonobj.units = 'degrees_east'
        var = f.createVariable(id,cdms2.CdDouble,(tobj,latobj,lonobj))
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
f = cdms2.createDataset('readonly.nc')
tobj = f.createAxis('time',numpy.array([time[0]]))
tobj.units = 'days since 2000-1-1'
latobj = f.createAxis('latitude',lat)
latobj.units = 'degrees_north'
lonobj = f.createAxis('longitude',lon)
lonobj.units = 'degrees_east'
var = f.createVariable('u',cdms2.CdDouble,(tobj,latobj,lonobj))
var.units = 'm/s'
var[:]=u[0]
mvar = f.createVariable('umasked',cdms2.CdDouble,(tobj,latobj,lonobj))
umask = numpy.ma.array(u[0])
umask[1] = numpy.ma.masked
mvar[:] = umask[:]
mvar.missing_value = umask.fill_value
f.close()

os.chmod('readonly.nc',0444)
