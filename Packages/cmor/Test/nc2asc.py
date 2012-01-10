#!/usr/bin/env python

import cdms2,sys, genutil
cdms2.setAutoBounds('on')

type = 'd'

order='zxty'
#order='xty'
#order = None
#order= 'txy'
var ='ta'
if len(sys.argv)>2:
	fnm=sys.argv[1]
	fout=sys.argv[2]
else:
	fnm='/ipcc/20c3m/atm/mo/%s/ncar_ccsm3_0/run1/%s_A1.20C3M_1.CCSM.atmm.1870-01_cat_1879-12.nc' % (var,var)
	fout='Test/%s.asc' % (var)

f=cdms2.open(fnm)

ntimes= 3
print 'var:',var
#s=f(var,time=slice(0,3),latitude=(-20,20),order=order,squeeze=1)
if order is not None:
	if order.find('z')>-1:
		s=f(var,time=slice(0,ntimes),order=order,squeeze=1,longitude=(-180,180,'con'),level=slice(5,12))
		print s.getLevel()[:]
	else:
		s=f(var,time=slice(0,ntimes),order=order,squeeze=1,longitude=(-180,180,'con'))
else:
	s=f(var,time=slice(0,ntimes),squeeze=1)
#s=s[:,::4,::4]
print 'Read in',s.shape
try:
	p=s.getLevel()
	p.id='pressure'
	p.units='Pa'
except:
	pass
#print genutil.minmax(s)
f.close()

f=open(fout,'w')

ndim=s.rank()
print 'Dumping'
print >>f, s.id
print >>f, s.units
print >>f, ndim

for i in range(ndim):
    ax=s.getAxis(i)
    print >>f, len(ax)
	
for i in range(ndim):
    ax=s.getAxis(i)
    if ax.isLatitude():
	    print >>f, 'latitude'
    elif ax.isLongitude():
	    print >>f,'longitude'
    else:
	    print >>f, ax.id
    print >>f, ax.units
    print  ax.id
    print  ax.units
    for j in ax[:]:
        print >>f, j,
    print >>f
    for j in ax.getBounds().flat[:]:
        print >>f, j,
    print >>f
f.flush()

s=s.filled(120).astype(type)
s=s.flat
j=0
for i in s[:]:
    print >>f, i,
    j+=1
print >>f
print j,s[-1]

f.close()
