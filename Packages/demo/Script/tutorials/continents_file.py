# Adapted for numpy/ma/cdms2 by convertcdms.py
# Import the modules needed for the tuturial
# cdms - Climate Data Management system accesses gridded data.
# vcs - Visualization and control System 1D and 2D plotting routines.
# cdutil - Climate utilitizes that contains miscellaneous routines for 
#          manipulating variables.
# time - This module provides various functions to mainpulate time values.
# os - Operation System routines for Mac, DOS, NT, or Posix depending on 
#      the system you're on.
# sys - This module provides access to some objects used or maintained by 
#       the interpreter and to functions that interact strongly with the interpreter.
import vcs, cdms2 as cdms, cdutil, time, os, sys

# Open data file:
filepath = os.path.join(vcs.prefix, 'sample_data/clt.nc')
cdmsfile = cdms.open( filepath )

# Extract a 3 dimensional data set and get a subset of the time dimension
data = cdmsfile('clt', longitude=(-180, 180), latitude = (-90., 90.))

# Initial VCS:
v = vcs.init()

# Show the list of continents attribute names.
v.show('continents')

# Assign the variable "ct_asd" to the persistent 'ASD' continents graphics methods.
ct_asd = v.getcontinents( 'ASD' )

# Plot only the the above continents graphics method.
v.plot( ct_asd )

# List the 'ASD' boxfill graphics methods attributes.
ct_asd.list()

# Change 'ASD' continents graphics methods attributes by entering the following commands.
ct_asd.line = 2            # set the line type
ct_asd.linecolor = 242     # set the line color
ct_asd.linewidth = 3       # set the line width
ct_asd.type = 3            # change the continents type to the 'Fine Continents'

# Clear the VCS canvas and plot data using the boxfill graphics method. Also use the predefined template 'ASD' and plot with no continents.
v.clear()
v.plot( data, 'ASD', continents=0 )

# Now overlay the 'ASD' continents using the 'ASD_dud' template. The 'ASD_dud' template omits all text and only plots the data.
v.plot( ct_asd, 'ASD_dud' )

# Change the continents attributes for better viewing
ct_asd.line = 0            # change to solid line
ct_asd.linecolor = 241     # change the line color to black
ct_asd.linewidth = 2       # change the line width to 2
ct_asd.type = 5            # change the continents type to the 'United States'

# Clear the VCS canvas and plot data using the boxfill graphics method. Also use the predefined template 'ASD' and plot with no continents.
v.clear()
v.plot( data, continents=4 )

# Now get the line 'continents' object.
lc = v.getline('continents')
lc.list()

# Change line attribute values
lc.color=250
lc.width=2

# Clear the canvas and plot changes
v.clear()
v.plot( data, continents=4 )
