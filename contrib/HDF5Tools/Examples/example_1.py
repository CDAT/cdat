print "While importing the module it will test for the presence of the 'h5dump' binary"
import HDF5Tools
path = './'

fnm = 'OMI-Aura_L2-OMAERUV_2007m0205t1530-o13622_v888-2007m0205t185330.he5'

print ' Open an HDF5 file'
HDF = HDF5Tools.HDF5(path+fnm)

print ' When opening an HDF5 it is scanned we can now look at variables in file'
vars = HDF.listvariables()
for v in vars:
    print 'Variable:',v

print ' Let link to one of the variables in this file'
AAD = HDF.variables['AerosolAbsOpticalDepth']

print "And let's examine it"
print "First of all its shape"
print AAD.shape
print "Let's look at its original 'group'"
print AAD._group
print 'Now its attributes'
print AAD._attributes
print "Let's look at the 'Title' attribute"
print AAD.Title

print "Finally let's read in the actual data"
v=AAD.get()
print v.shape
print 'Again attributes are attached to the MV returned'
print v.Title

print 'v can also be obtained with the following 2 calls'
print 'from the file variable but no need to type get:'
v=AAD()
print 'Or straight from the file, not the difference with [], variable is now actually loaded in memory'
v=HDF('AerosolAbsOpticalDepth')

print '''Note this MV has no dimensions/axes associated with it, merely the attributes that were present in the file, the HDF5_OMI class demonstrate how you can develop more powerful tools for specific HDF5 file types, HDF5_OMI classes can most likely be easily adapted to other data types w/o touching the code'''
