import numpy
import cdms2
import os

from markError import clearError,markError,reportError
clearError()
cdms2.setNetcdfClassicFlag(0)

print 'Test 18: numpy types with NetCDF4 ...',

for t in [numpy.byte,numpy.short,numpy.int,numpy.int32,numpy.float,numpy.float32,numpy.double,numpy.ubyte,numpy.ushort,numpy.uint,numpy.int64,numpy.uint64]:
    print 'Testing type:',t
    data = numpy.array([0], dtype=t)
    var = cdms2.createVariable(data)
    f = cdms2.open('test_%s.nc'%data.dtype.char, 'w')
    f.write(var, id='test')
    f.close()
    f=cdms2.open('test_%s.nc'%data.dtype.char)
    s=f("test")
    f.close()
    os.unlink("test_%s.nc" % data.dtype.char)

print 'Done'
cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)
var = cdms2.createVariable(numpy.array([0], dtype=numpy.int64))
f = cdms2.open('test.nc', 'w')
try:
    f.write(var, id='test')
except Exception,err:
    markError("Writing Netcdf4 type to NetCDF3 format")
os.unlink("test.nc")
f.close()
reportError()

