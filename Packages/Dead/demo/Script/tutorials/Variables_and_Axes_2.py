# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2, MV2, numpy

# First let's create a variable, type flaot, named 'myvar'
var = MV2.array([[1,2,3], [4,5,6],[7,8,9], [10,11,12] ],typecode='f', id='myvar')

# Now let's assume this is lat/lon variable
# Storing the number of lat/lon
nlat,nlon=var.shape

# Step 1, creating the longitude axis
# First let's create the values
# First longitude located at 0
lons = numpy.arange(0,360,360./nlon,'f')
print lons          # [   0., 120., 240.,]

# Now create the axis
lons=cdms2.createAxis(lons)

# And decorate it
lons.id = 'longitude'
lons.units = 'degrees_east'

# Just to make sure we will describe this axis as a longitude one
lons.designateLongitude()

# Now let's do the same for the latitudes
# First latitude located at 180./nlat/2. from south pole
lats = numpy.arange(-90,90,180./nlat,'f') + 90./nlat
print lats           # [-67.5,-22.5, 22.5, 67.5,]

# Now create the axis
lats=cdms2.createAxis(lats)

# And decorate it
lats.id = 'latitude'
lats.units = 'degrees_north'

# Just to make sure we will describe this axis as a latitude one
lats.designateLatitude()

# Now let's apply these to the variable
# We can do it one at a time:
var.setAxis(0,lats)
var.setAxis(1,lons)

# Or all at once:
var.setAxisList((lats,lons))

# And let's write the variable to  a file
f=cdms2.open('out.nc','w')
f.write(var)
f.close()
