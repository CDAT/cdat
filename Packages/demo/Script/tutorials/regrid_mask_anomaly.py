# Import modules
import cdms2, cdutil, vcs, cdtime
import string, numpy, time, MV2, sys, os
from regrid2 import Regridder

# Open data file
file1 = os.path.join(sys.prefix, 'sample_data/hadcrut2_sample.nc')
a = cdms2.open(file1) 
print a.listvariable()

# get the start and end time steps 
start_time = a.getAxis('time').asComponentTime()[0]
end_time = a.getAxis('time').asComponentTime()[-1]
# get the data 
data=a('temanom',time=(start_time, end_time),latitude=(-90,90))
print data.shape

x=vcs.init()
x.setcolormap('default')
x.plot(data)

# get grid for regridding 
grid1=data.getGrid() 
# see how it looks like
print grid1.info()

# get "spatial missing mask"  
mask1=data.mask
# get metadata for final desired data (e.g. latitudes,longitudes,time)
lat=data.getLatitude()
lon=data.getLongitude()
tim=data.getTime()
# close the file
a.close()

# Get the ERA40 data for both 2-meter temperature (tas) and ssts (sst)
# on original grid

file2 = os.path.join(sys.prefix, 'sample_data/era40_tas_sample.nc')
b = cdms2.open(file2) 
# get data
print "reading 'tas' data, please wait..."
tas=b('tas')
print tas.shape 
# close the file
b.close()
# now get the 'sst' data
file3 = os.path.join(sys.prefix, 'sample_data/era40_sst_sample.nc')
b = cdms2.open(file3)
# get data
print "reading 'sst' data, please wait..."
sst=b('sst')
b.close()  
print sst.shape

x.clear() 
x.plot(sst)

y=vcs.init()
y.setcolormap('default')
y.plot(tas)

# get grid for regridding 
grid2=sst.getGrid()

# setup a regridding function (as: fromgrid, togrid)
regridfunc=Regridder(grid2,grid1)
# create new data with 'togrid' (5-deg) resolution by passing
# the data with 'fromgrid' resolution into the funstion above
sst_new=regridfunc(sst)
tas_new=regridfunc(tas)

x.clear() 
x.plot(sst_new)

y.setcolormap('default')
y.plot(tas_new)

# extract a land/sea mask and regrid it to our desired 5 degree grid
# (these data are percent land coverage [0-100])
file4 = os.path.join(sys.prefix, 'sample_data/geo.1deg.ctl')
c = cdms2.open(file4) 
fraction=c('sftlf',squeeze=1)
c.close() 

# plot the fraction field
y.setcolormap('default')
y.plot(fraction)

# get grid for regridding 
grid3=fraction.getGrid() 
# etup regrid function 
regridfunc=Regridder(grid3,grid1)
# regrid mask values 
fraction=regridfunc(fraction) 
  
# create land and sea masks.
# 50% or more coverage in a box is defined as land 
# and less than or equal to 50% coverage is ocean.
# All other values in the arrays are zeros.
land=numpy.where(numpy.greater(fraction.filled(),50.),1.,0.)
ocean=numpy.where(numpy.less_equal(fraction.filled(),50.),1.,0)

masked_sst=numpy.multiply(sst_new.filled(),ocean)
masked_tas=numpy.multiply(tas_new.filled(),land)

x.clear() 
x.plot(masked_sst)

y.clear() 
y.plot(masked_tas)

# add land and ocean contributions for the merged product 
merged=masked_sst+masked_tas
# add metadata to this numeric array

merged=cdms2.createVariable(merged,axes=(tim,lat,lon), 
             typecode='f',id='merged_tas_sst') 
merged.id='merged_tas_sst' 
merged.set_fill_value(1e20)  
cdutil.setTimeBoundsMonthly(merged)

x.clear() 
x.plot(merged)

# write out the total temperature data to a netcdf file 
o=cdms.open('era40_merged_tas_sst.nc','w')
o.write(merged)

# crete base period 1991-1993, inclusive 
start_time = cdtime.comptime(1991,1,1)
end_time   = cdtime.comptime(1993,12,1)

# the annualcycle 

ac=cdutil.ANNUALCYCLE.climatology( merged( time =  
        (start_time, end_time, 'co')))
# use the defined annual cycle and generate anomalies
merged_an=cdutil.ANNUALCYCLE.departures(merged,ref=ac)

# add metadata to the new anomaly variable  

merged_an=cdms.createVariable(merged_an,axes=(tim,lat,lon),
          typecode='f',id='anomalies_merged_tas_sst') 
merged_an.id='anomalies_merged_tas_sst'
 # Lastly apply the "spatial missing mask" to these data 
merged_an=MV2.masked_where(MV2.equal(mask1,1),merged_an)

y.clear() 
y.plot(merged_an)

o.write(merged_an)
o.close()

