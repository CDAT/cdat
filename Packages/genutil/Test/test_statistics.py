#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

import cdms2,genutil,os,sys

f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))

u=f('u',time=slice(0,1),level=slice(0,1),squeeze=1)
v=f('v',time=slice(0,1),plev1=slice(0,1),squeeze=1)


print u.shape,v.shape

print 'Lagged correlation'
print '1',genutil.statistics.laggedcorrelation(u,v,axis=0)
print '2',genutil.statistics.laggedcorrelation(u,v,axis=0,lag=4)
print '3',genutil.statistics.laggedcorrelation(u,v,axis=0,lag=4,noloop=1).shape
print '4',genutil.statistics.laggedcorrelation(u,v,axis=0,lag=[4,8,10]).shape

print 'Lagged covariance'
print '1',genutil.statistics.laggedcovariance(u,v,axis=0)
print '2',genutil.statistics.laggedcovariance(u,v,axis=0,lag=4)
print '3',genutil.statistics.laggedcovariance(u,v,axis=0,lag=4,noloop=1).shape
print '4',genutil.statistics.laggedcovariance(u,v,axis=0,lag=[4,8,10]).shape

print 'Auto correlation'
print '1',genutil.statistics.autocorrelation(u,axis=0)
print '2',genutil.statistics.autocorrelation(u,axis=0,lag=4)
print '3',genutil.statistics.autocorrelation(u,axis=0,lag=4,noloop=1).shape
print '4',genutil.statistics.autocorrelation(u,axis=0,lag=[4,8,10]).shape

print 'Auto covariance'
print '1',genutil.statistics.autocovariance(u,axis=0)
print '2',genutil.statistics.autocovariance(u,axis=0,lag=4)
print '3',genutil.statistics.autocovariance(u,axis=0,lag=4,noloop=1).shape
print '4',genutil.statistics.autocovariance(u,axis=0,lag=[4,8,10]).shape

