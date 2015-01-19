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
data = cdmsfile('clt', longitude=(-180, 180), latitude = (-50., -50.))

# Initial VCS:
v = vcs.init()

# Plot data using the default Yxvsx graphics method:
#
# Here is an example of using the Yxvsx graphics method
# that makes an X-Y plot (where the y-axis is a function
# of the x-axis).
#
v.yxvsx( data, long_name ='Simple X-Y Plot' )

# Plot data using the default Xyvsy graphics method:
#
# Here is an example of using the Xyvsy graphics method
# that makes an X-Y plot (where the x-axis is a function
# of the y-axis).
#
v.clear()
v.xyvsy( data, long_name ='Simple X-Y Plot' )
