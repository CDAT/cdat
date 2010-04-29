# File: example1.py
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
# In this example, we perform a pre-PCA to reduce the number of
# d-o-f, then we perform an MSSA to extract the first oscillation
# (first par of modes). Finally, we compute phase composites
# to represent the oscillation over its cycle.
###################################################################
print "#################################################"
print "# PCA+MSSA+Phase_composites of 1st oscillation. #"
print "# Then reconstructions, and plots:              #"
print "# - 1 phase over 2                              #"
print "# - 1 time series                               #"
print "#################################################"

###############################################
# Initialize and get data
###############################################

# Needed modules
print 'Importing needed modules...'
# - base
import sys,cdms2 as cdms,MV2 as MV,numpy as N,cdutil,genutil,os
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

# Retrieve data
print "Read the whole dataset"
s=f('ssta')


###############################################
# Use spanlib
###############################################

# Create the analysis object
print "Creating SpAn object"
npca=4
SP=spanlib.SpAn(s,npca=npca)

# Perform a preliminary PCA+MSSA
# (equivalent to simple use SP.mssa(pca=True) later)
print "PCA..."
SP.pca()
eof = SP.pca_eof()
pc = SP.pca_pc()
ev = SP.pca_ev()


f=cdms.open('out.nc','w')
f.write(s,extend=False)
f.write(eof)
f.write(pc)
f.write(ev)
f.close()

import pylab as P
P.figure(figsize=(8,10))
for im in xrange(npca):
	P.subplot(npca*2,2,im*2+1)
	P.plot(eof[im].filled())
	P.title('EOF %i'%(im+1))
	P.subplot(npca*2,2,im*2+2)
	P.plot(pc[im].filled())
	P.title('PC %i'%(im+1))
P.savefig('out.png')






raise 'a'

from actimar.misc.plot import map,curve
#map(SP.pca_eof()[0],resolution='c')
curve(SP.pca_pc()[0])

sys.exit()
# MSSA on PCA results
print 'MSSA...'
SP.mssa()

# Phase composites of first two MSSA modes
print 'Phase composites...'
composites = SP.reconstruct(phases=True,nphases=6,imode=-2)


###############################################
# Plots
###############################################

print "Now, plot!"
nslice = composites.shape[0]
ncol = 3
nrow = (nslice-1)/ncol+1

# Try matplotlib because it is nicer and easier than vcs
try:
	from pylab import *
	from matplotlib.toolkits.basemap import Basemap

	def bwr():
		cdict = {'red': ((0.,)*3,(.5,1.,1.),(1.,1.,1.)),
			'green': ((0.,)*3,(.5,1.,1.),(1.,0.,0.)),
			'blue': ((0.,1.,1.),(.5,1.,1.),(1.,0.,0.))}
		return matplotlib.colors.LinearSegmentedColormap('bwr',cdict,256)

	figure(figsize=(8,8))
	subplot(ncol,nrow,nslice)
	for i in xrange(nslice):
		lon = composites.getLongitude()
		lat = composites.getLatitude()
		xx,yy = meshgrid(lon,lat)
		m = Basemap(resolution='l',lat_0=N.average(lat),lon_0=N.average(lon),
			llcrnrlon=min(lon),llcrnrlat=min(lat),
			urcrnrlon=max(lon),urcrnrlat=max(lat))
		levels = vcs.mkscale(genutil.minmax(composites))
		m.contourf(lon,lat,composites[i],levels=levels,cmap=bwr)
		clabel(m.contour(lon,lat,composites[i],levels=levels))
		title("Phase %i/%i" (i+1,6))
		m.drawcoastlines()
		m.drawcountries()
		m.fillcontinents(color='coral')
		m.drawparallels(vcs.mkscale(genutil.minmax(lat)),labels=[1,0,0,0])
		m.drawmeridians(vcs.mkscale(genutil.minmax(lon)),labels=[0,0,0,1])
	figtext(.5,1.,"\n%s [%s]" % ("El Nino phase composites\nSea surface temperature", composites.units))
	savefig(sys.argv[0].replace(".py",".png"))
	show()
	

# Fall back to vcs because we have it!
except:
	# Plot 1 phase over two, then a time series
	# TODO: we must do something nicer!!
	import vcs,EzTemplate
	x=vcs.init()
	T=EzTemplate.Multi(rows=nrow,columns=ncol) # Nrow added 1 for original data row
	mn,mx=-1,1
	levels = vcs.mkscale(mn,mx)
	levels.insert(0,-1.e20) # extension left side
	levels.append(1.e20) # extension right side
	colors = vcs.getcolors(levels)
	iso = x.createisofill('spanlib')
	iso.levels = levels
	iso.fillareacolors = colors
	f=cdms.open('tmp.nc','w')
	f.write(out,id='composites',typecode='f')
	f.close()
	for i in xrange(nslice):
		print i
		templ = T.get(font=0)
		x.plot(out[i],templ,iso,ratio='autot',bg=1,title="Phase composites of the first MSSA oscillation")
	x.postscript('crap')
	x.showbg()
	raw_input('map out ok?')
	x.clear()
	x.plot(out[:,30,80],title="Cycle of the ocillation")
	raw_input('Time series at center of bassin ok?')
	x.clear()
