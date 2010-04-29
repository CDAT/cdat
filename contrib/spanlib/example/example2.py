# File: example2.py
#
# This file is part of the SpanLib library.
# Copyright (C) 2006  Charles Doutiraux, Stephane Raynaud
# Contact: stephane dot raynaud at gmail dot com
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA


###################################################################
# In this example, we analyse two different areas at the same time.
# You can do the same with two completely different datasets,
# except that they must have the same temporal grid.
###################################################################
print "##############################################"
print "# PCA+MSSA applied on two different regions. #"
print "# Then reconstructions and plots.            #"
print "##############################################"

# Needed modules
print 'Importing needed modules...'
# - base
import cdms,MV,vcs,sys,os
# - current version of spanlib is prioritary
if os.path.exists('../src/build/tmp_lib'):sys.path.insert(0,'../src/build/tmp_lib')
import spanlib

# We tell cdms that we have longitude, latitude and time
cdms.axis.latitude_aliases.append('Y')
cdms.axis.longitude_aliases.append('X')
cdms.axis.time_aliases.append('T')

# Simply open the netcdf file
print "Open file"
f=cdms.open('data2.cdf')

# Get our two datasets
print "Read two different regions"
s2=f('ssta',latitude=(-10,10),longitude=(110,180))
s1=f('ssta',latitude=(-15,15),longitude=(210,250))
s1[:,0:10,0:4]=MV.masked
s2[:,0:5,0:6]=MV.masked
print MV.average(s1.flat),MV.average(s2.flat)

# Stack the two dataset to have only one dataset
print "Stacking data"
res = spanlib.stackData(s1,s2)

# Create the analysis object
print "Creating SpAn object"
SP=spanlib.SpAn(MV.array(res[0]),weights=MV.array(res[1]))

# Perform a preliminary PCA
# (optional step since done by default with mssa)
print "PCA+MSSA..."
steof,stpc,stev = SP.mssa(pca=True)

# Recontructed the filtered field
print "Reconstructing all modes"
ffrec = SP.reconstruct()

# Unstacking
print "Unstaking data"
out = spanlib.unStackData(ffrec,res[1],res[2],res[3])

# Plot a timeseries taken from our two
# recontructed datasets
# TODO: we must do something nicer!!
print "Time series for the two filtered regions"
x=vcs.init()
x.plot(out[1][:,5,5])
x.plot(out[0][:,5,5])
raw_input('ok?')
