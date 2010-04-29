## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

#!/usr/bin/env python

print 'Test 2: CdmsFile I/O ... ',

import cdms2,numpy,string,os,sys
from numpy.ma import masked
from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

f = cdms2.open(os.path.join(sys.prefix,'sample_data','readonly.nc'))
u = f.variables['u']
umasked = f.variables['umasked']

try:
    s = u.size()
except:
    markError("Size")
else:
    if s!=512: markError("Size",s)

uslice = u[:,4:12,8:24]
fullu = u[:]
uslicemask = umasked[:,0:4,8:24]
comp = x[0,0,4:12,8:24]
compmask = x[0,0,0:4,8:24]
compmask[1]=masked

lonaxis = f['longitude']
if not numpy.ma.allequal(lonaxis.getValue(), lonaxis[:]): markError("Axis getValue()")

if not numpy.ma.allequal(uslice,comp): markError("Slice read")
if not numpy.ma.allequal(uslicemask,compmask): markError("Slice read, masked")
if lonaxis.isVirtual(): markError("virtual axis test")

if u.units!='m/s': markError("Attribute read: "+u.units)

t = u.getTime()
if not numpy.ma.allequal(t[:],[   0.]): markError("Axis read")
if not t.units=="days since 2000-1-1": markError("Axis attribute: "+t.units)

grid = u.getGrid()
if grid.id!='grid_16x32': markError("Grid id",grid.id)

try:
    os.unlink('junk.nc')
except:
    pass
g = cdms2.open('junk.nc','a')
xx = u.subSlice()
g.write(xx)
g.close()

uwrap = u.subRegion(longitude=(-180,180))
if uwrap.getGrid() is None: markError("Wraparound does not preserve the grid")

f.close()

try:
    badslice = u[:,4:12,8:24]
    badslice = u[0:1]
    badu = u.getValue()
except cdms2.CDMSError, e:
    i = string.find(str(e),"Cannot read from closed")
    if i!=0: markError("Handling read from closed file")
else:
    markError("Handling read from closed file")
    
try:
    badslice = u[0:1]
except cdms2.CDMSError, e:
    i = string.find(str(e),"Cannot read from closed")
    if i!=0: markError("Handling read from closed file")
else:
    markError("Handling read from closed file")
    
try:
    badu = u.getValue()
except cdms2.CDMSError, e:
    i = string.find(str(e),"Cannot read from closed")
    if i!=0: markError("Handling read from closed file")
else:
    markError("Handling read from closed file")
    
try:
    u[0,0,0] = -99.9
    u[0:1]=-99.9
    u.setValue(fullu)
except cdms2.CDMSError, e:
    i = string.find(str(e),"Cannot write to a closed")
    if i!=0: markError("Handling write to a closed file")
else:
    markError("Handling write to closed file")

reportError()
