# Adapted for numpy/ma/cdms2 by convertcdms.py
import numpy
import cdms2, cdutil, vcs, cdtime
import string, time, sys, os
#
# A program that loops through a subset of the AMIP data
# for a specified variable.  Data are extracted covering the AMIP I
# time period (197901-198412) and average annual cycle is calculated
# and gridpoint anomalies and a global anomaly time series
# is generated.
#
# We will work with two smaller subsets of model data
# We assume to have the data from CCSR/NIES AGCM model (tas_ccsr-95a_*.nc)
# and another model data from DNM model (tas_dnm-95a_*.nc)
# To use those files as dataset we can create the xml description file
# by runnindg at a shell prompt:
#    cdscan -x tas_ccsr-95a.xml tas_ccsr-95a*.nc
# and separate for the second dataset:
#    cdscan -x tas_dnm-95a.xml tas_dnm-95a*.nc
# We will use the *.xml files that are created for you in the sample data
# directory of your cdat installation
#

# variable we want to work with
var='tas'
#
# models we will be using
model=['ccsr-95a', 'dnm-95a' ]
#
# set up a description string for addition to the global
# attributes in the output netcdf
#
model_description=''
# loop over all models--similar to a fortran do loop
for i in range(0,len(model)):
 file_xml = os.path.join(vcs.prefix,'sample_data/'+var+'_'+model[i]+'.xml')
 a=cdms2.open(file_xml)
 data=a[var]
 print '----  ', i, model[i],data.shape
 start_time = data.getTime().asComponentTime()[0]
 end_time = data.getTime().asComponentTime()[-1]
 print 'start time: ',start_time,'  end time:',end_time
 time_len = len(data.getTime())
 print 'time axis lenght: ', time_len
 a.close()
 dm=str(i)+' = '+model[i]
 model_description=model_description+', '+dm

#
print;print '__________________';print
# set up an output array for the global time series
glan=numpy.ma.zeros([len(model),time_len],numpy.float)
#
# Loop over the files and read data into memory.  Subtract the average
# annual cycle and area-average the departure maps for a global departure/anomaly
# time series.
#
#start_time = cdtime.comptime(1979,2,1)
#end_time   = cdtime.comptime(1984,12,1)
#
for i in range(0,len(model)):
 file_xml = os.path.join(vcs.prefix,'sample_data/'+var+'_'+model[i]+'.xml')
 a=cdms2.open(file_xml)
 data=a(var,time=(start_time,end_time),squeeze=1)
 a.close()
 ac=cdutil.ANNUALCYCLE.climatology(data(time=(start_time, end_time, 'cob')))
 data_an=cdutil.ANNUALCYCLE.departures(data,ref=ac)
 print i,model[i],data.shape, data_an.shape
 glan[i,:]=cdutil.averager(data_an,axis='xy')

#
# setup metadata and write out to a netcdf file
#
tim=data.getTime()
runs=numpy.arange(0,len(model))
runs=cdms2.createAxis(runs,id='models')
glan=cdms2.createVariable(glan,axes=(runs,tim),id='global_'+var+'_anomalies')
#
print 'start write file..'
q=cdms2.open('global_anomalies.nc','w')
q.model_designation=model_description
q.write(glan)
q.close()
#
# a simple plot
#
x=vcs.init()
x.setcolormap('default')
x.plot(glan)

