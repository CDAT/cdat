#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

import cdms2 as cdms,genutil,vcs,cdtime,os,sys
import vcs.test.support
bg=vcs.test.support.bg
cdms.setAutoBounds('on')
#f=cdms.open('/pcmdi/obs/mo/ta/rnl_ncep/ta.rnl_ncep.ctl')
f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','ta_ncep_87-6-88-4.nc'))

levels = [1000,700,800]
try:
    s=f('ta',slice(0,1),genutil.picker(level=levels,match=1))
    error = False
except:
    print 'Ok failed getting the missing levels as expected!'
    error = True
if error == False:
    raise Exception,"Error it should have failed here!"


s=f('ta',slice(0,1),genutil.picker(level=levels,match=0))

if s.shape[1]!=3:
    raise "Error did not return 3 levels!"
if (s.getLevel()[:]!=levels).any():
    raise Exception,"Error did not retrieve the right levels!"

print "folowing plot should show all missing values, since 800 does not exisits!"
x=vcs.init()
x.plot(s[0,-1],bg=bg)
vcs.test.support.check_plot(x)

levels = [1000,700,850]
s3=f('ta',genutil.picker(time=['1987-7','1988-1',cdtime.comptime(1988,3)],level=[1000,700,850]))

if s3.shape!=(3, 3, 73, 144):
    raise Exception,"Did not retrieve the right slab"
t1= cdtime.componenttime(1987,7)
t2= cdtime.componenttime(1988,1)
t3= cdtime.componenttime(1988,3)
if s3.getTime().asComponentTime()!=[t1,t2,t3]:
    raise Exception,"Error did not get the right times"
test = s3.getLevel()[:]!=levels
if test.any():
    raise Exception,"Error did not get the right levels"
