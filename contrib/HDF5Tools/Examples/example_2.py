import HDF5Tools, vcs
path = './'

fnm = 'OMI-Aura_L2-OMAERUV_2007m0205t1530-o13622_v888-2007m0205t185330.he5'

print ' Open an HDF5 file, but this time using the OMI class, this is a particular type of HDF5/EOS files'
HDF = HDF5Tools.HDF5_OMI(path+fnm)

print ' When opening an HDF5/OMI it is scanned we can now look at variables in file, but also it separates them between actual variables and dimensions variables, by default dimension are setup as having a group containing "Geolocation Fields" but this can redefined by passing the keyword "dimension_kw" on the opening commandf line (see below)'
print 'We can now list the actual variables and their shape and group:'
vars = HDF.listvariables()
for v in vars:
    print 'Variable:',v,HDF.variables[v].shape,HDF.variables[v]._group
print 'And the dimensions ones that have been separated with "dimension_kw" keyword'
dims = HDF.listdimension()
for d in dims:
    print 'Dimension Var',d,HDF.variables[d].shape

print "Let's pass another keyword for demonstrating the dimension_kw"
HDF = HDF5Tools.HDF5_OMI(path+fnm,dimension_kw='Geolocation')
print 'Leads to the them result of course'
dims = HDF.listdimension()
for d in dims:
    print 'Dimension Var',d,HDF.variables[d].shape

print "Let's link to one of the variables in this file"
AAD = HDF.variables['AerosolAbsOpticalDepth']

print "And let's examine it"
print "First of all its shape"
print AAD.shape
print 'Now its attributes'
print AAD._attributes
print "Note that it now has an attribute called 'coordinates' this points to the lat/lon variable. By default (None) it will be set to the OMI standard 'Latitude Longitude', but you can redefine it, either while opening the file (keyword coordinates) or by setting manually this attribute yourself."
print "WARNING: when redefining the attribute be sure to put the latitude variable first and THEN the longitude variable"
print "Example of redefinition"
HDF = HDF5Tools.HDF5_OMI(path+fnm,coordinates='Latitude Longitude')
AAD = HDF.variables['AerosolAbsOpticalDepth']
print AAD.coordinates

print 'Or:'
HDF = HDF5Tools.HDF5_OMI(path+fnm)
AAD = HDF.variables['AerosolAbsOpticalDepth']
AAD.coordinates = 'Latitude Longitude'
print AAD.coordinates

print 'This attribute is VERY important indeed, when defined it will be used to map the data onto a curvilinear grid'
print 'This grid is defined via the keyword (opening time or retrieving time) "resolution_lat/lon" which defines the total extend of the mesh around the lat/lon coordinate'

print "So let's get this variable"
aad = AAD() # or AAD.get()
print aad.shape
print "This variable can now be manipulated via cdat, which knows its lat/lon info"
print "example, let's get only one of the last two level/band and restrain the data to a -20,20 domain in latitude"
aad = aad[...,0,1](latitude=(-20,20))
print aad.shape
print 'And plot it'
x=vcs.init()
m = x.createmeshfill('omi')
x.plot(aad,m)
raw_input('Press enter')

print 'Ok another way to change the resolution is either at calling time or opening time, both time using the "resolution" keyword'

aad = AAD(resolution_lon=.25,coordinates='Latitude Longitude')

print 'But you do not have to read the mesh info each and everytime, you can actually pass the grid to the call for another variable, saving you the time to read again lat,lon and creating the grid, see example 4'

