#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py

"""
#*******************************************************************************
# 
# This tutorial demonstrates usage of some basic statistical functions in the
# genutil module. This requires installation of the genutil module (part of the
# standard distribution). 
# 
#*******************************************************************************
"""

from genutil import statistics
import cdms2, MV2
import os, sys


#*******************************************************************************
#
# Printing the documentation or "doc" strings. for covariance
#
#*******************************************************************************
print statistics.covariance.__doc__



#*******************************************************************************
#
# Example 1:
#           We compute the spatial rms difference between 2 fields. The 2 fields
#           chosen here are the surface air temperature fields in the NCEP/NCAR
#           reanalysis for the 1960-1969 and 1980-1989 periods.
#
#*******************************************************************************

#
# First of all let us define our 2 periods of interest
#
import cdtime
#
# Period 1 with start at 1980 and end at 1985
#
a1 = cdtime.comptime(1980)
b1 = cdtime.comptime(1985)
#
# By default, the definition above sets the time at January 1st of the years
# specified.
#
# Similarly, Period 2 will start at 1990 and end at 1995
#
a2 = cdtime.comptime(1990)
b2 = cdtime.comptime(1995)

#
# Let us retrieve data for surface air temperature (tas)
# for each of these 2 periods we just defined.
#
ncep = os.path.join(sys.prefix, 'sample_data', 'tas_mo.nc')
f = cdms2.open(ncep)

ncep1 = f('tas', time = (a1, b1, 'co'))
ncep2 = f('tas', time = (a2, b2, 'co'))
#
# Note that by using the 'co' option above we specified that the end
# point is 'open'
#

#
# Now we compute the "spatial" rms difference -
# "spatial" is specified by setting the axis='xy' option
#
rms = statistics.rms(ncep1, ncep2, axis='xy')
print 'RMS difference (spatial) between the 2 periods:'
print rms

#
# Similarly the "mean absolute difference" is computed...
#
absd = statistics.meanabsdiff(ncep1,ncep2,axis='xy')
print 'Mean absolute difference:'
print absd


#*******************************************************************************
#
# Example 2:
#           In this example, we compute the spatial correlation between 2 fields
#           - say the mean surface air temperature for the 1980-1989 period
#           versus each month of the 1980-1989 period.
#
#*******************************************************************************

#
# First compute the average for the 1980-1990 period (designated ncep2 above)
#
mncep2 = MV2.average(ncep2)

#
# And now correlate this pattern with each month of ncep2
#
cor = statistics.correlation(ncep2, mncep2, axis='xy')
print 'Correlation:'
print cor


#*******************************************************************************
#
# Example 3:
#           We take the correlation example above a step further and specify
#           that the correlation be computed with weights based on latitude
#           area taken into account. Note that the correlation can also be
#           weighted over axes of our choice.
#*******************************************************************************

#
# To compute the latitude area weights, we take the difference of the sine of
# the latitude bounds for each latitude point in the grid. First let us get the
# bounds of the Latitude axis
#
lat = ncep2.getLatitude()

bnd = lat.getBounds()


#
# Taking the sine of it
#
import numpy
bnd = numpy.sin(bnd/180.0 * numpy.pi)


#
# And create the difference between the sine of bounds
#

w = []
for i in range(len(bnd)):
    b = bnd[i]
    w.append(b[1]-b[0])
# end of for b in bnd:


#
# The difference w is a Python list now. Let us make it a "variable"
#
w = MV2.asVariable(w)

#
# So w is a variable with 1 dimension (of same length as 'lat') 
# Now we set its axis to be the same as "lat" (Latitude axis in ncep2)
#
w.setAxis(0, lat)


#
# Now let us compute the correlation again.
# Remember that w is 1D only. Since the other 2 dimensions are 
# equally weighted, there is no need to explicitly make w 3D
# the statistic code will do it for you
#
wcor = statistics.correlation(ncep2, mncep2, weights=w, axis='xy')
print 'Weighted correlation:'
print wcor

