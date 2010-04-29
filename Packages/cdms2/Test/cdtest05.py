## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

#!/usr/bin/env python

import cdms2,numpy,cdtime,os,sys
from cdms2 import MV2 as MV
from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

print 'Test 5: get/sub, time functions ...',

f = cdms2.open(os.path.join(sys.prefix,'sample_data','test.xml'))
v = f.variables['v']
vp = x[1,1:,4:12,8:24]
wp = x[1,2,4:12]
xp = numpy.ma.concatenate((x[1,1:,4:12,8:NLON],x[1,1:,4:12,0:8]),axis=2)

# getRegion - positional

s = v.getRegion((366.,731.,'ccn'),(-42.,42.,'ccn'),(90.,270.,'con'))
if not numpy.ma.allequal(vp,s): markError('getRegion/positional failed')

# getRegion - keyword
s = v.getRegion(latitude=(-42.,42.,'ccn'),longitude=(90.,270.,'con'),time=(366.,731.,'ccn'))
if not numpy.ma.allequal(vp,s): markError('getRegion/keyword failed')

# getRegion - wraparound
s4 = v.getRegion(time=(366.,731.,'ccn'),latitude=(-42.,42.,'ccn'),longitude=(90.,450.,'con'))
if not numpy.ma.allequal(xp,s4): markError('getRegion wraparound failed')

# getSlice - positional
s = v.getSlice(2,(4,12),Ellipsis,squeeze=0)
if not numpy.ma.allequal(wp,s): markError('getSlice/positional failed')

# getSlice - keyword
s = v.getSlice(latitude=(4,12), time=2)
if not numpy.ma.allequal(wp,s): markError('getSlice/keyword failed')

# subRegion - positional
s2 = v.subRegion((366.,731.,'ccn'),(-42.,42.,'ccn'),(90.,270.,'con'))
if not numpy.ma.allequal(vp,s2): markError('subRegion/positional failed')

# subRegion - squeeze
try:
    s2 = v.subRegion((731.,731.,'ccn'),(-42.,42.,'ccn'),(90.,270.,'con'),squeeze=1)
except IndexError:
     markError('subRegion squeeze option failed')

# mf 20010308 subRegion - extended wrap
fw = cdms2.open(os.path.join(sys.prefix,'sample_data','ps.wrap.test.0E.nc'))
ps = fw.getVariable('ps')
ps1 = ps[:,:,36:]
ps2 = ps[:,:,:37]
s2 = numpy.ma.concatenate((ps1,ps2),axis=2)
s2w = fw('ps',longitude=(-180,180,'ccn'))
if not numpy.ma.allequal(s2,s2w): markError('subRegion extended wrap')
varlist = fw.getVariables(spatial=1)

u = f['u']
u1 = u[:,:,8:]
u2 = u[:,:,:8]
ucat = MV.concatenate((u1,u2),axis=2)
su = u.subRegion(lon=(90,450,'co'))
if not numpy.ma.allequal(ucat,su): markError('subRegion wrap, test 2')

# negative strides
fc = cdms2.Cdunif.CdunifFile(os.path.join(sys.prefix,'sample_data','ps.wrap.test.0E.nc'))
psc = fc.variables['ps']
psb = psc[:]
s3c = psb[0,::-1]
s4c = psb[0,::-2]
s3 = fw('ps',latitude=(90,-90))
if not numpy.ma.allequal(s3,s3c): markError('Reverse interval failed')

s4 = ps.getSlice(':',(None,None,-1))
# s4 = ps.subRegion(latitude=slice(None,None,-1))
if not numpy.ma.allequal(s4,s3c): markError('Negative stride failed')
s5 = ps.getSlice(':',(None,None,-2))
if not numpy.ma.allequal(s5,s4c): markError('Negative stride=2 failed')
s6w = fw('ps',longitude=(180,-180,'ccn'))
if not numpy.ma.allequal(s2[...,::-1],s6w): markError('subRegion extended wrap')

fw.close()
fc.close()

# subRegion - keyword
s2 = v.subRegion(latitude=(-42.,42.,'ccn'),longitude=(90.,270.,'con'),time=(366.,731.,'ccn'))
if not numpy.ma.allequal(vp,s2): markError('subRegion/keyword failed')

# subRegion - order
al = s2.getAxisList()
result = cdms2.avariable.order2index(al, 'yxt')
if not result == [1,2,0]: markError('order2index 1 failed.')
result = cdms2.avariable.order2index(al, '...(longitude)t')
if not result == [1,2,0]: markError('order2index 2 failed.')
result = cdms2.avariable.order2index(al, '...20')
if not result == [1,2,0]: markError('order2index 3 failed.')
result = cdms2.avariable.order2index(al, '1...')
if not result == [1,0,2]: markError('order2index 4 failed.')
s3 = s2(order='yxt', squeeze=1)
if not s3.getOrder() == 'yxt': markError('subRegion/order failed')
s2 = v.subRegion(latitude=(-42.,42.,'ccn'),longitude=(90.,270.,'con'),time=(366.,731.,'ccn'), order='yxt')
if not numpy.ma.allequal(s3,s2): markError('subRegion/order 2 failed')

# subRegion - time types
s2 = v.subRegion(latitude=(-42.,42.,'ccn'),longitude=(90.,270.,'con'),time=('2001-1','2002-1','ccn'))
if not numpy.ma.allequal(vp,s2): markError('subRegion/time string failed')

t1 = cdtime.comptime(2001)
#
#  mf change to pull exactly the second time
#
t2 = cdtime.comptime(2002)
s2 = v.subRegion(latitude=(-42.,42.,'ccn'),longitude=(90.,270.,'con'),time=(t1,t2))
if not numpy.ma.allequal(vp,s2): markError('subRegion/component-time failed')

t1 = cdtime.comptime(2003)
t2 = cdtime.comptime(2004)
try:
    s2 = v.subRegion(latitude=(-42.,42.,'ccn'),longitude=(90.,270.,'con'),time=(t1,t2))
    markError('subRegion/component-time extending beyond valid interval (should not have worked!!) (mf) ')
except:
    pass

t1 = cdtime.reltime(0,"years since 2001")
t2 = cdtime.reltime(1,"year since 2001")
s2 = v.subRegion(latitude=(-42.,42.,'ccn'),longitude=(90.,270.,'con'),time=(t1,t2,'ccn'))
if not numpy.ma.allequal(vp,s2): markError('subRegion/component-time failed')

try:
    xx = v.subRegion('2000')
except:
    markError('subRegion/individual time string failed')

# subSlice - positional
s3 = v.subSlice(2,(4,12),Ellipsis,squeeze=0)
if not numpy.ma.allequal(wp,s3): markError('subSlice/positional failed')

# subSlice - keyword
s3 = v.subSlice(latitude=(4,12),time=2)
if not numpy.ma.allequal(wp,s3): markError('subSlice/keyword failed')

# subSlice - squeeze
try:
    s3 = v.subSlice(latitude=(4,12),time=2, squeeze=1)
except IndexError:
     markError('subSlice squeeze option failed')
# require
try:
    s3 = v.subSlice(required='time')
    s3 = v.subSlice(required=('time','latitude'))
except:
     markError('required selector failed')
try:
    s3 = v.subSlice(required='lumbarsupport')
except cdms2.CDMSError:
    pass
except:
    markError('required selector failed')
try:
    s3 = v.subSlice(required='lumbarsupport')
except cdms2.SelectorError:
    pass
except:
    markError('required selector failed')
try:
    varlist = f.getVariables(spatial=1)
except:
    markError('getVariables(spatial=1) failed')

f.close()

# Time functions

def isEqual(x,y):
    return (abs(y-x)< ((1.e-7)*max(abs(y),1.)))

def cmpYear(c1,c2):
    return ((c1.year == c2.year) and (c1.month == c2.month) and (c1.day == c2.day)) or ((c1.year == c2.year == 1582) and (c1.month == c2.month == 10) and (c1.day in [5,15] and (c2.day in [5,15])))

def testCal(a,b,c,d,e,f,g,cal=cdtime.MixedCalendar):
    x = cdtime.comptime(d,e,f)
    units = "days since %d-%d-%d"%(a,b,c)
    r = x.torel(units,cal)
    if not isEqual(r.value,g):
        markError('component => relative failed: %s %s'%(`x`,`r`))
    r2 = cdtime.reltime(g, units)
    x2 = r2.tocomp(cal)
    if not cmpYear(x,x2):
        markError('relative => component failed: %s %s'%(`r2`,`x2`))
    units2 = "days since %d-%d-%d"%(d,e,f)
    r3 = cdtime.reltime(10.0,units2)
    r4 = r3.torel(units)
    if not isEqual(r4.value,(10.0+g)):
        markError('relative => relative: %s %s'%(`r3`,`r4`))
    bb = cdtime.comptime(a,b,c)
    x2 = bb.add(g,cdtime.Day,cal)
    if not cmpYear(x,x2):
        markError('component add failed: %s %s'%(`bb`,`x2`))
    x2 = x.sub(g,cdtime.Day,cal)
    if not cmpYear(bb,x2):
        markError('component sub failed: %s %s'%(`x`,`x2`))
    r2 = cdtime.reltime(g, units)
    r3 = r2.add(1000.0,cdtime.Day,cal)
    if not isEqual(r3.value, g+1000.0):
        markError('relative add failed: %s %s'%(`r2`,`r3`))
    r3 = r2.sub(1000.0,cdtime.Day,cal)
    if not isEqual(r3.value, g-1000.0):
        markError('relative sub failed: %s %s'%(`r2`,`r3`))

testCal(1,1,1,1582,1,1,577460.00)
testCal(1,1,1,1582,1,1,577460.00,cdtime.JulianCalendar)
testCal(1582,1,1,1,1,1,-577460.00)
testCal(1,1,1,1970,1,1,719164.0)
testCal(1970,1,1,1,1,1,-719164.0)
testCal(1583,1,1,1970,1,1,141349.00)
testCal(1970,1,1,1583,1,1,-141349.00)
testCal(1970,1,1,1583,1,1,-141349.00,cdtime.GregorianCalendar)
testCal(1582,10,4,1582,10,15,1.0)
testCal(1582,10,15,1582,10,4,-1.0)

reportError()
