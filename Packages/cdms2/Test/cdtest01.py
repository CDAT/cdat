## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

#!/usr/bin/env python

# Test dataset I/O

print 'Test 1: Dataset I/O ... ',

import cdms2,numpy,string,os,sys
from cdms2.variable import WriteNotImplemented
from cdms2.avariable import NotImplemented
from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

f = cdms2.open(os.path.join(sys.prefix,'sample_data','test.xml'))
if f.id!='test': markError('File id',f.id)
u = f.variables['u']
try:
    scalar = u[0,0,0]
except:
    markError("Scalar slice")
try:
    s = u.size()
except:
    markError("Size")
else:
    if s!=1536: markError("Size",s)
fullu = u[:]

uslice = u[:,4:12,8:24]
comp = x[0,:,4:12,8:24]
if not numpy.ma.allequal(uslice,comp): markError("Slice read")

u2 = u[0:-1]
comp2 = x[0,0:-1]
if not numpy.ma.allequal(u2,comp2): markError("Negative index slice",u2.shape)

if u.units!='m/s': markError("Attribute read: "+u.units)

t = u.getTime()
if not numpy.ma.allequal(t[:],[   0., 366., 731.,]): markError("Axis read")
if not t.units=="days since 2000-1-1": markError("Axis attribute: "+t.units)
if t.isVirtual(): markError("virtual axis test")

grid = u.getGrid()
if grid.id!='grid_16x32': markError("Grid id",grid.id)

# Test extended write
out = cdms2.open("testExtendWrite.nc",'w')
v = f.variables['v']
u0 = u.subSlice(0)
u1 = u.subSlice(1)
u2 = u.subSlice(2)
v0 = v.subSlice(0)
v1 = v.subSlice(1)
v2 = v.subSlice(2)
uout = out.write(u0)
vout = out.write(v2, attributes=v.attributes, id='v', extend=1, index=2)
out.write(u1,index=1)
out.write(v0)
out.write(u2)
out.write(v1)
out.sync()
tout = out.axes['time']
try:
    trev = tout[::-1]
except:
    markError("Read with negative stride")
uoutar = uout.getSlice()
if not numpy.ma.allclose(u[:], uoutar): markError("Extended write")

out.close()

# Test strides across the partitioned dimension
try:
    x = u[0:3:2,0:16:2,0:32:2]
except:
    markError("Strides across partitioned dimension")

f.close()

try:
    badslice = u[:,4:12,8:24]
    badu = u.getValue()
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
    badslice = u[0:1]
except cdms2.CDMSError, e:
    i = string.find(str(e),"Cannot read from closed")
    if i!=0: markError("Handling read from closed file")
else:
    markError("Handling read from closed file")

try:
    u[0,0,0]=-99.9
except cdms2.CDMSError, e:
    if str(e)!=WriteNotImplemented: markError("Handling write to dataset")
else:
    markError("Handling write to dataset")
    
try:
    u[0:1]=-99.9
except cdms2.CDMSError, e:
    if str(e)!=WriteNotImplemented: markError("Handling write to dataset")
else:
    markError("Handling write to dataset")
    
try:
    u.assignValue(fullu)
except cdms2.CDMSError, e:
    i = string.find(str(e),NotImplemented)
    if i!=0: markError("Handling write to dataset")
else:
    markError("Handling write to dataset")

os.unlink("testExtendWrite.nc")

reportError()
