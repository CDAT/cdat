# Adapted for numpy/ma/cdms2 by convertcdms.py
# Run with the -i option: "python -i animate.py"
# in the interactive python mode

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
data = cdmsfile('clt',\
        time=('1980-1-1 0:0:0.0', '1980-12-1 0:0:0.0'),\
        longitude=(-180, 180), latitude = (-90., 90.))

# Initialize VCS:
v = vcs.init()

# Plot data using the default isofill graphics method:
v.isofill( data )

# Create the images required for animation
# The "thread_it" option waits for the animation to be created before
# moving onto the next command.
v.animate.create( thread_it = 0 )

# Run the animation using the images created
v.animate.run( )

# Stop the animation
v.animate.stop( )

# Run the animation and pause between frames
v.animate.run( )
v.animate.pause( 3 )

# Zoom in on the animated frames
v.animate.zoom( 2 )

# Move the animation horizonatally to the up and down
v.animate.horizontal( 50 )

# Move the animation vertically left and right
v.animate.vertical( 50 )

# Stop the animation and view frame 5, 10, and 15
v.animate.stop( )
v.animate.frame( 3 )
v.animate.frame( 5 )
v.animate.frame( 10 )

# Control the animate via the animation GUI
v.animate.gui( )
