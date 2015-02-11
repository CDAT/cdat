# Import modules
import cdms2, cdutil, vcs, cdtime
import string, time, MV2, sys, os
from regrid2 import Regridder
from genutil import statistics


file1 = os.path.join(vcs.prefix, 'sample_data/era40_tas_sample.nc')
f1 = cdms2.open( file1 )

f1.showvariable()
f1.listdimension()

print  f1.getAxis('time').asComponentTime()[0]
# 1990-1-1 0:0:0.0
print  f1.getAxis('time').asComponentTime()[-1]
# 1993-12-1 0:0:0.0

file2 = os.path.join(vcs.prefix, 'sample_data/era15_tas_sample.nc')
f2 = cdms2.open( file2 )

f2.showvariable()
f2.listdimension()

print  f2.getAxis('time').asComponentTime()[0]
# 1989-1-1 0:0:0.0
print  f2.getAxis('time').asComponentTime()[-1]
# 1994-2-1 0:0:0.0

# get data with overlapping in overlapping time range
data1 = f1('tas', time = ('1991-1-1','1993-12-1'))
data2 = f2('tas', time = ('1991-1-1','1993-12-1'))

print data1.shape
# (48, 160, 320)
print data2.shape
# (48, 73, 144)


grid1=data1.getGrid()
print grid1
print 'original ERA40 data shape: ',data1.shape
# original ERA40 data shape:  (48, 160, 320)

grid2 = data2.getGrid()
print grid2


regridfunc=Regridder(grid1,grid2)
data1=regridfunc(data1)
print 'new ERA40 data shape: ' ,data1.shape

cdutil.setTimeBoundsMonthly(data1)
cdutil.setTimeBoundsMonthly(data2)

start_time = cdtime.comptime(1991,1,1)
end_time   = cdtime.comptime(1993,12,1)

ac1=cdutil.ANNUALCYCLE.climatology(data1(time=(start_time, end_time, 'cob')))
ac2=cdutil.ANNUALCYCLE.climatology(data2(time=(start_time, end_time, 'cob')))
print ac1

data1=cdutil.ANNUALCYCLE.departures(data1,ref=ac1)
data2=cdutil.ANNUALCYCLE.departures(data2,ref=ac2)

print data1.shape,data2.shape

tim = data2.getTime()
lat=data2.getLatitude()
lon=data2.getLongitude()

data1=cdms2.createVariable(data1,axes=[tim,lat,lon],typecode='f',id='tas')

diff=MV2.subtract(data1,data2)
# zonal differences
z_diff=MV2.average(diff,2)
print 'Zonal data shape (before): ',z_diff.shape

z_diff=MV2.transpose(z_diff,(1,0))

# add id to data
z_diff.id='zonal_diff'
print 'Zonal data shape (after): ',z_diff.shape

# global differences
gl_diff=cdutil.averager(diff,axis='xy')

x=vcs.init()
x.setcolormap('default')
fill=x.getisofill('default')
x.plot(z_diff,fill)

x.clear()
x.plot(gl_diff)

cor_t=statistics.correlation(data1,data2,axis='xy')
# temporal correlation map betwen these to time-series
cor_m=statistics.correlation(data1,data2,axis='t')
# temporal rms difference between the two time series
rms=statistics.rms(data1,data2,axis='t')

x.clear()
x.plot(cor_m,fill)

x.clear()
x.plot(cor_t)

x.clear()
x.plot(rms,fill)

slope1, intercept1 = statistics.linearregression(data1, axis='t')
slope2, intercept2 = statistics.linearregression(data2, axis='t')
# set the 'id'
slope1.id='linear_slope'
slope2.id='linear_slope'

dec_trnd_diff=(slope1*120.)-(slope2*120.)
dec_trnd_diff.id='decadal_trend_diff'

x.clear()
x.plot(dec_trnd_diff)

variance1=statistics.variance(data1)
variance2=statistics.variance(data2)
variance1.id='variance_data1'
variance2.id='variance_data2'

x.clear()
x.plot(variance1,fill)

x.clear()
x.plot(variance2,fill)

f=variance1/variance2
f.id='variance_data1_dividedby_variance_data2'

o=cdms2.open('tas_comparison.nc','w')
o.write(f)
o.write(variance1)
o.write(variance2)
o.write(dec_trnd_diff)
o.write(rms)
o.write(z_diff)
o.close()

f1.close()
f2.close()

