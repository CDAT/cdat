# Adapted for numpy/ma/cdms2 by convertcdms.py
import adamsregrid
import numpy
import EzTemplate
import vcs.test.support
bg  = vcs.test.support.bg

ts=[]
M = EzTemplate.Multi(1,5)
for i in range(5):
    ts.append(M.get())

## Prepare axes

lon1 = numpy.arange(0,360,.25,'f')/180.*numpy.pi #.25 deg
lat1 = numpy.arange(0,180,2,'f')/180.*numpy.pi #2 deg
lev1 = numpy.arange(0,17,1,'f')+1. # 17 levs
tim1 = numpy.arange(0,24,3,'f')+1. # 3hourly data

lon2 = numpy.arange(0,360,5,'f')/180.*numpy.pi #5 deg
lat2 = numpy.arange(0,180,4,'f')/180.*numpy.pi #4 deg
lev2 = numpy.arange(0,17,4,'f')+1. # less levs
tim2 = numpy.arange(0,24,6,'f')+1. # 6hourly data

p1 = numpy.cos(lon1)
p2 = p1[numpy.newaxis,:]*numpy.sin(lat1)[:,numpy.newaxis]
p3 = p2[numpy.newaxis,:,:]*lev1[:,numpy.newaxis,numpy.newaxis]
p4 = p3[numpy.newaxis,:,:,:]*tim1[:,numpy.newaxis,numpy.newaxis,numpy.newaxis]


print 'Testing for 1D array/grid'
interps = ['linear','linearLog','cubic','cubicLog']
M.x.clear()
M.x.plot(p1,ts[0],bg=bg)
for i in range(4):
    interp = interps[i]
    R = adamsregrid.Regrid(lon1,lon2,interp,0)

    po1 = R.rgrd(p1)
    M.x.plot(po1,ts[i+1],bg=bg)
vcs.test.support.check_plot(M.x)
    
print 'Testing for 2D array/grid'
interps = ['linear','linearLog','cubic','cubicLog']
M.x.clear()
M.x.plot(p2,ts[0],bg=bg)
for i in range(4):
    interp = interps[i]
    R = adamsregrid.Regrid(lon1,lon2,interp,1,
                           lat1,lat2,interp,0)

    po2 = R.rgrd(p2)
    M.x.plot(po2,ts[i+1],bg=bg)
vcs.test.support.check_plot(M.x)

print 'Testing for 3D array/grid'
interps = ['linear','linearLog','cubic','cubicLog']
M.x.clear()
M.x.plot(p3,ts[0],bg=bg)
for i in range(4):
    interp = interps[i]
    R = adamsregrid.Regrid(lon1,lon2,interp,2,
                           lat1,lat2,interp,1,
                           lev1,lev2,interp,0,
                           )

    po3 = R.rgrd(p3)
    M.x.plot(po3,ts[i+1],bg=bg)
vcs.test.support.check_plot(M.x)

print 'Testing for 4D array/grid'
interps = ['linear','linearLog','cubic','cubicLog']
M.x.clear()
M.x.plot(p4,ts[0],bg=bg)
for i in range(4):
    interp = interps[i]
    R = adamsregrid.Regrid(lon1,lon2,interp,3,
                           lat1,lat2,interp,2,
                           lev1,lev2,interp,1,
                           tim1,tim2,interp,0,
                           )

    po4 = R.rgrd(p4)
    M.x.plot(po4,ts[i+1],bg=bg)
vcs.test.support.check_plot(M.x)

print 'Testing for 1D array/grid passing 2D'
interps = ['linear','linearLog','cubic','cubicLog']
M.x.clear()
M.x.plot(p2,ts[0],bg=bg)
for i in range(4):
    interp = interps[i]
    R = adamsregrid.Regrid(lon1,lon2,interp,1)

    po2 = R.rgrd(p2)
    M.x.plot(po2,ts[i+1],bg=bg)
vcs.test.support.check_plot(M.x)


