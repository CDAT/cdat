# Adapted for numpy/ma/cdms2 by convertcdms.py
# Import the modules needed for the tuturial
import vcs, cdms2 as cdms, cdutil, time, os, sys

# Open data file:
filepath = os.path.join(sys.prefix, 'sample_data/clt.nc')
cdmsfile = cdms.open( filepath )

# Extract a 3 dimensional data set and get a subset of the time dimension
data = cdmsfile('clt', longitude=(-180, 180), latitude = (-90., 90.))

# Initial VCS:
v = vcs.init()

# Assign the variable "bf_asd" to the persistent 'ASD' boxfill graphics methods.
bf_asd = v.getboxfill( 'ASD' )

# Assign the variable "cf_asd" to the persistent 'ASD' isofill graphics methods.
cf_asd = v.getisofill( 'ASD' )

# Assign the variables "bf_asd1" and "bf_asd2" to the persistent 
# boxfill graphics methods.
tplt_asd1 = v.gettemplate( 'ASD1_of_2' )
tplt_asd2 = v.gettemplate( 'ASD2_of_2' )

# Plot the data using the above graphics methods and templates.
# Plot the data in background mode using the "bg=1" option.
# In this example two plot will be plotted on the VCS Canvas.
v.plot( data, bf_asd, tplt_asd1, bg=1 )
v.plot( data, cf_asd, tplt_asd2, bg=1 )

v.postscript('test.ps')
v.gif('test.gif')
v.cgm('test.cgm')
#v.raster('test.raster')# This only works when a VCS Canvas is displayed.

v.gs('test.png')# generate ghostscript png output file

# generate ghostscript tiff output file
v.gs('test.tif', device = 'tiff24nc', orientation = 'l', resolution = '172.x172.')