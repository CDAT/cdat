# Adapted for numpy/ma/cdms2 by convertcdms.py
# Import the modules needed for the tuturial
import vcs, cdms2 as cdms, cdutil, time, os, sys

# Open data file:
filepath = os.path.join(sys.prefix, 'sample_data/clt.nc')
cdmsfile = cdms.open( filepath )

# Extract two 3 dimensional data sets and get a subset of the time dimension
data1 = cdmsfile('u', longitude=(-180, -48.75), latitude = (10., 70.43))
data2 = cdmsfile('v', longitude=(-180, -48.75), latitude = (10., 70.43))

# Set the longitude and latitude axes of the "v" variable
# to that of the "u" variable.
data2.setAxis(2, data1.getAxis(2))
data2.setAxis(3, data1.getAxis(3))

# Initial VCS:
v = vcs.init()

# Show the list of persistent scatter graphics methods.
v.show( 'scatter' )

# Assign the variable "sf_asd" to the persistent 'ASD' scatter graphics methods.
sf_asd = v.getscatter( 'ASD' )

# Plot the data using the above scatter graphics method.
v.plot( data1, data2, sf_asd )

# List the 'ASD' scatter graphics methods attributes.
sf_asd.list()

# Change the scatter attributes
sf_asd.marker = 5.0
sf_asd.markercolor = 242
sf_asd.markersize = 15

# Create a persistent scatter graphics methods from an existing scatter graphics method.
sf_new = v.createscatter( 'new', 'ASD' ) # create new from ASD
sf_new2 = v.createscatter( 'new2', 'ASD' )# create new2 from ASD
sf_new.list()             # list its attributes
v.show( 'scatter' )       # show vector list with new and new2
v.removeobject( sf_new )  # remove new from vector list
v.show( 'scatter' )       # show vector list without new
