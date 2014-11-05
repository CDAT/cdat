# Adapted for numpy/ma/cdms2 by convertcdms.py
# Import the modules needed for the tuturial
import vcs, cdms2 as cdms, cdutil, time, os, sys

# Open data file:
filepath = os.path.join(vcs.prefix, 'sample_data/clt.nc')
cdmsfile = cdms.open( filepath )

# Extract a 3 dimensional data set and get a subset of the time dimension
data = cdmsfile('clt', longitude=(-180, 180), latitude = (-90., 90.))

# Initial VCS:
v = vcs.init()

# Assign the variable "t_asd" to the persistent 'ASD' template.
t_asd = v.gettemplate( 'ASD' )

# Create a new template from the existing 'ASD' template
t2_asd = v.createtemplate( 'new', 'ASD' )

# Plot the data using the above 'ASD' template.
v.plot( data, t_asd )

# Remove picture segments from the page.
t_asd.list( )
t_asd.xlabel2.priority = 0
t_asd.xtic2.priority = 0
t_asd.xtic2.priority = 0
t_asd.legend.priority=0

# save current 'Mean' placemant for x and y coordinates
xmean_current = t_asd.mean.x
ymean_current = t_asd.mean.y

# now change the placement
t_asd.mean.x=0.5       # move the "Mean" text to x-axis center
t_asd.mean.y=0.5       # move the "Mean" text to y-axis center

t_asd.data.priority = 0 # remove the data so the "Mean" text is visable.
v.update()

#############################################################################
# Place the colorbar legend vertically and to the right side                                       #############################################################################
t_asd.data.priority = 1
t_asd.legend.priority = 1
t_asd.legend.list()         # list the legend members
v.mode=0                    # turn the automatic update off

# move 'Mean' text back where it was
t_asd.mean.x = xmean_current
t_asd.mean.y = ymean_current

# move the right side of a plot to the left to make space for the legend
# first move the inner plot
t_asd.data.x2 = 0.87
# then move the sorrounding box - the right y-axis
t_asd.box1.x2 = 0.87

# set the top x-axis (secind y axis) to be blank
t_asd.xlabel2.priority = 0
t_asd.xtic2.priority = 0
# set the right y-axis (second y axis) to be blank (priority=0)
t_asd.ylabel2.priority = 0
t_asd.ytic2.priority = 0

# move the colorbar legend position, to be vertial and to the right
t_asd.legend.x1=0.9
t_asd.legend.y1=0.82
t_asd.legend.x2=0.95
t_asd.legend.y2=0.3

# clear the canvas and plot the template again
v.clear()
v.plot( data, t_asd )


