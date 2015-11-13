import cdms2
import cdutil
import pdb
import numpy.ma


f=cdms2.open("http://dataserver2.nccs.nasa.gov/thredds/dodsC/bypass/CREATE-IP/MERRA/mon/atmos/va.ncml")

var=f['va']
var=var[0:24,0,:]

operator = cdutil.SEASONALCYCLE
result = operator.climatology(var).squeeze()
resultClim = [ numpy.ma.average(result[i,:]) for i in range(4) ]

result2 =  operator.climatology(var,unsafe=True)

print resultClim
print result2
