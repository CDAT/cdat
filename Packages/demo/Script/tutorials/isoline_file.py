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

# Show the list of persistent isoline graphics methods.
v.show('isoline')

# Assign the variable "df_asd" to the persistent 'ASD' isoline graphics methods.
df_asd = v.getisoline( 'ASD' )

# Plot the data using the above isolien graphics method.
v.plot( data, df_asd )

# List the 'ASD' isoline graphics methods attributes.
df_asd.list()

# change the isofill levels and line type and color
df_asd.levels = ( [20,0], [30,0], [40,0], [50,0], [60,0])
df_asd.levels = ( [0,20],[20,40],[50,60])
df_asd.levels = ( [20,0], [30,0], [40,0], [50,0], [60,0])
df_asd.levels = (30,50,70)
df_asd.line=[0, 2, 0]
df_asd.linecolors=(16,100,200)

# view the level labels
df_asd.label='y'

# set the label font and text color
df_asd.text=(1, 5, 9)
df_asd.textcolors=(16,100,200)

# Create a persistent isoline graphics methods from an existing isoline graphics method.
cf_new = v.createisoline( 'new', 'ASD' ) # create new from ASD
cf_new2 = v.createisoline( 'new2','quick' )# create new2 from quick
cf_new.list()                        # list its attributes
v.show('isoline')                    # show isoline list with new and new2
v.removeobject( cf_new )             # remove new from isoline list
v.show('isoline')                    # show isoline list without new
