

#!/usr/bin/env python

# Can't write a _FillValue attribute to a Variable
# J-Y Peterschmitt - LSCE - 07/2015
import cdms2,numpy,cdtime,os,sys
import numpy as np
from markError import NTIME,NLAT,NLON,x,clearError,markError,reportError
clearError()

data = np.random.random((10, 10))
data = np.ma.array(data,fill_value=1234.0)
data = np.ma.masked_less(data, 0.5)
dummy_grid = cdms2.createUniformGrid(10, 10, 1, 0, 10, 1)
dummy_var = cdms2.createVariable(data,
                                 axes=[dummy_grid.getLatitude(),
                                       dummy_grid.getLongitude()],
                                 id='my_var')

if dummy_var.fill_value != 1234.0:   markError("createVariable fill_value failed")

# Test if all fillvalue related attributes are changed
dummy_var.fill_value= 2.e+20
if (dummy_var.fill_value != 2.e20)  or \
   (dummy_var._FillValue != 2.e20)  or \
   (dummy_var.missing_value != 2.e20) :   markError("fill_value property failed (value 2.e20)")

dummy_var._FillValue=1.33
if (dummy_var.fill_value != 1.33)  or \
   (dummy_var._FillValue != 1.33)  or \
   (dummy_var.missing_value != 1.33) :   markError("fill_value property failed (value 1.33)")

dummy_var.dummy_att = 'Dummy att'
dummy_var._dummy_att = 'Dummy att starting with _'
dummy_var.units = '10e3 dummy'


cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

# Test 1 passing fill_value
var_att = dummy_var.attributes
f = cdms2.open('dummy1.nc', 'w')
f.write(dummy_var, fill_value=999.999)
f.close()

f = cdms2.open('dummy1.nc')
m=f("my_var")
if (m._FillValue != 999.999) or \
   (m.missing_value != 999.999): markError("fill_value property failed in dummy1.nc")

# Test 2 Copy dummy_var attributes
var_att = dummy_var.attributes
var_att['another_att'] = 'New att'
var_att['_another_att'] = 'New att starting with _'

f = cdms2.open('dummy2.nc', 'w')
f.write(dummy_var, attributes=var_att)
f.close()

f = cdms2.open('dummy2.nc')
m=f("my_var")
if (m._FillValue != 1.33) or \
   (m.missing_value != 1.33): markError("fill_value property failed in dummy2.nc")
# Test 3 pass variable as is
dummy_var = cdms2.createVariable(data,
                                 axes=[dummy_grid.getLatitude(),
                                       dummy_grid.getLongitude()],
                                 id='dummy3_var')

f = cdms2.open('dummy3.nc', 'w')
f.write(dummy_var)
f.close()

f = cdms2.open('dummy3.nc')
m=f("dummy3_var")
if (m._FillValue != 1234.0) or \
   (m.missing_value != 1234.0): markError("fill_value property failed in dummy2.nc")
# Test 3 pass variable as is
# The end
os.remove("dummy1.nc")
os.remove("dummy2.nc")
os.remove("dummy3.nc")
reportError()
