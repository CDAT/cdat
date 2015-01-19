# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,sys
import cdat_info

f=cdms.open(cdat_info.get_prefix()+'/sample_data/clt.nc')

# Query "file" attributes
print f.listglobal()

print f.Conventions

# Now query the file for variables
print f.listvariables()

# We can also query for dimensions
print f.listdimension()

# To query a variable without actually loading it
# in memory first, use a "File Variable" by using []
# Here we create the file variable pointing to 'clt'
# And query it for all available info
V=f['clt']

# This was just a print statement, we can actually
# query the variable for specifics
print V.listattributes()

print V.listdimnames()

# To query a dimension you can either get it
# from the file or from the variable
#time=f['time']
#time.getAxis(0)
#time=V.getTime()
# Time, Level, Latitude, Longitude can be retrieve
# directly independently of where they are in
# the variable, using getTime, getLevel, getLatitude,
# or getLongitude()
