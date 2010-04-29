# Adapted for numpy/ma/cdms2 by convertcdms.py
# Import the modules needed for the tuturial
import vcs, cdms2 as cdms, cdutil, time, os, sys

# Open data file:
filepath = os.path.join(sys.prefix, 'sample_data/clt.nc')
cdmsfile = cdms.open( filepath )


# Extract 3 dimensional data sets and get a subset of the time dimension
data = cdmsfile( 'clt')
data1 = cdmsfile('u', longitude=(-180, -50), latitude = (10., 72))
data2 = cdmsfile('v', longitude=(-180, -50), latitude = (10., 72))
data3 = cdmsfile('clt', 
longitude=(-180, -50), latitude = (10., 70))

# Set the longitude and latitude axes of the "v" variable
# to that of the "u" variable.
data2.setAxis(2, data1.getAxis(2))
data2.setAxis(3, data1.getAxis(3))

# Initial VCS:
v = vcs.init()

# 1st plot the data with the isofill graphics method.
v.isofill( data )

# 2nd overlay the data with the isoline graphics method.
v.isoline( data )

# Clear the VCS Canvas
v.clear( )

# Plot the variable clt with the boxfill graphics method, 
# then the u and v vector as an overlay. Because it 
# is an overlay we do not want to plot any text or 
# continents. To only plot data, use the "default_dud" 
# template.
v.boxfill( data3 )
v.vector( data1, data2, 'default_dud', continents=0 )


