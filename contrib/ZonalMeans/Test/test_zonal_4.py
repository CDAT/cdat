# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2
import sys,MV2
import ZonalMeans

delta_lon=.5
delta_lat=.5
nlat=int(180./delta_lat)
nlon=int(360/delta_lon)
s=MV2.ones((nlat,nlon),typecode='f')
print delta_lon,nlon,delta_lat,nlat
g=cdms2.createUniformGrid(-89.5, nlat, delta_lat, 0., nlon, delta_lon)
s.setAxis(-1,g.getLongitude())
s.setAxis(-2,g.getLatitude())
s.setGrid(g)
print s.shape
print s.getGrid()
print s.getLongitude()
print s.getLatitude()
zm=ZonalMeans.compute(s,delta_band=5)
print zm

