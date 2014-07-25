# Adapted for numpy/ma/cdms2 by convertcdms.py
"""This is a set of tests for averager - which can be used to calculate area weighted averages."""
import cdms2, MV2, numpy, numpy.ma, cdtime, os, sys
from cdutil import area_weights, averager, AveragerError
cdms2.setAutoBounds('on')
f=cdms2.open(os.path.join(sys.prefix,'sample_data','tas_ukmo_con.nc'))
x = f('tas')
ans = averager(x, axis='yt', weights = ['weighted','equal'], combinewts=1)
ans = averager(x, axis='yt', weights = ['weighted','equal'], combinewts=1)
#


ans2 = averager(x, axis='yx', weights = [numpy.ones(len(x.getLatitude()), numpy.float), 'generate'], combinewts=1)


b = numpy.array([1.,2.3, 3.0, 4.3])
b1 = numpy.ma.array(b)

b2,w = numpy.ma.average(b1, weights=None, returned=1)
assert numpy.ma.allclose(b2, 2.65)

b2b,wb = averager(b1, weights=None, returned=1)
assert numpy.ma.allclose(b2b, 2.65)
assert numpy.ma.allclose(wb, w)


ba = averager(b)
assert numpy.ma.allclose(ba, 2.65)


bs = averager(b, action='sum')
assert numpy.ma.allclose(bs, 10.6)



b = numpy.ones((3,4,5))
c = averager(b, weights=None, axis=[0,2], action='sum')
assert numpy.ma.allclose(c, [ 15., 15., 15., 15.,])
assert isinstance(c, numpy.ndarray)

try:
    c2 = averager(b, weights=numpy.ones(b.shape), axis=[0,2], action='sum')
    raise Exception('Averager did not fail as it should have')
except AveragerError:
    pass


try:
    c2 = averager(b, weights=numpy.ones((3,5,4)), axis=[0,2], action='sum')
    raise Exception('Averager did not fail as it should have')
except AveragerError:
    pass


d = averager(b, weights='equal', axis=2, action='sum')

d0MA = numpy.ma.average(b, weights=numpy.ma.array([1.,2.,3.,4.,5.]), axis=2)
d0 = averager(b, weights=numpy.ma.array([1.,2.,3.,4.,5.]), axis=2) 
assert numpy.ma.allclose(d0, d0MA)


d1 = averager(b, weights=numpy.ma.array([1.,2.,3.,4.,5.]), axis=2, action='sum')

d2 = averager(b, weights=[numpy.ma.array([1.,2.,3.]),'equal'], axis=[0,2], action='sum')


b = numpy.array([1.,2.3, 3.0, 4.3])
bs = averager(b, action='sum')
assert numpy.ma.allclose(bs, 10.6)  


b = numpy.ones((3,4,5))
c = averager(b, weights=None, axis=[0,2], action='sum')
assert numpy.ma.allclose(c, [ 15., 15., 15., 15.,])
assert isinstance(c, numpy.ndarray)

b1 = numpy.ma.array(b)
c1 = averager(b1, weights=None, axis=[0,2])
assert isinstance(c1,numpy.ndarray)

averager(x, axis = 'xyt')

ans = averager(x)

averager(x, axis='2t')

averager(x, axis='t(lat)')

try:
    averager(x, axis='t(lev)')
    raise RuntimeError( "Test did not fail as it should.")
except AveragerError:
    pass


try:
    averager(x, axis='t3')
    raise RuntimeError( "Test did not fail as it should.")
except AveragerError:
    pass

averager(x, axis='t')

averager(x, axis='tx', weight=['generate', 'generate']) 

averager(x, axis='tx', weight=['equal', 'generate']) 

try:
    averager(x, axis='tx', weight='equal') 
    raise RuntimeError( "Test did not fail as it should.")
except AveragerError:
    pass

try:
    a = numpy.array(range(10), numpy.float)
    averager(x, axis='tx', weight=['equal', a]) 
    raise RuntimeError( "Test did not fail as it should.")
except AveragerError:
    pass

try:
    b=numpy.ma.array(a)
    averager(x, axis='tx', weight=['equal', b]) 
    raise RuntimeError( "Test did not fail as it should.")
except AveragerError:
    pass

result = averager(x, axis='tx', weight=['equal', 'equal'])

result = averager(x, axis='2t', weight=['generate', 'equal'])


#**********************************************************************
#
# Create the area weights 
#
aw = area_weights(x)
#
#
#**********************************************************************


result = averager(x, axis='x', weight=aw)


result = averager(x, axis='xy', weight=aw) 


#
# Now I want the Longitude axis to be area weighted (including any missing data)
# but the latitude axis to be equally weighted
#
result, newwts = averager(x, axis='x', weight=aw, returned=1) 
new_result = averager(result, axis='y', weight='equal')
result = averager(x, axis='21', weight=aw) 

result = averager(x, axis=0) 

#******************************************************************************************
# Now a real world check!!
#******************************************************************************************

#
# The values in this file were calculated using old routines in CDAT2.4
#
fcheck = cdms2.open(os.path.join(sys.prefix,'sample_data','tas_gavg_rnl_ecm.nc'))
start = cdtime.componenttime(1979, 1, 1)
end =  cdtime.componenttime(1979, 12, 1)
correct = fcheck('tas', time=(start, end))

#
# The source for the averages in the above file is the raw data in the file below.
# The difference in units (degrees C and degrees K) should be the obvious answer.
#
f = cdms2.open(os.path.join(sys.prefix,'sample_data','tas_ecm_1979.nc'))

#
# I can invoke averager by passing the f('tas') as the variable instead of first extracting
# the variable...... Youcan obviously get more fancy with the kind of selected variables you pass.
#
result = averager(f('tas'), axis='xy', weight=['generate', 'generate'])


#
# For the purposes of this test, I am using the extracted variable as below
#
x = f('tas')


#
# I am going to calculate the answer in 2 ways.
#
# First method: Use the 'generate' options to average over x and y
#
result1 = averager(x, axis='xy', weight=['generate', 'generate'])

#
# Second Method: Create the area weights and
#                convert it into an MV2 before passing to the averager.
#
aw = area_weights(x)
aw = cdms2.createVariable(aw, axes=x.getAxisList())
result2 = averager(x, axis='x(lat)', weight=aw)


#
# Third Method: Use the area weights from Charles' function created above
#               but call the averaging only one 1 dimension at a time. Use the average &
#               weights from the first step in the averaging at the second step!.
#
temp_step, temp_wts = averager(x, axis='x', weight=aw, returned=1)
result3 = averager(temp_step, axis='y', weight=temp_wts)

#
# Note that the above way of doing multiple steps is useful when you want the temp_wts
# altered between steps.......

#
# Now check the 3 results....... they will be different by 273.0 (Centigrade to Kelvin)
#
diff1 = result1 - correct
assert MV2.allclose(273.0, diff1)
diff2 = result2 - correct
assert MV2.allclose(273.0, diff1)
diff3 = result3 - correct
assert MV2.allclose(273.0, diff1)
assert MV2.allclose(result1, result2)
assert MV2.allclose(result2, result3)

#
# This test is to verify the action='sum' option
#
tasm_file = os.path.join(sys.prefix,'sample_data','tas_cru_1979.nc')

ftasm = cdms2.open(tasm_file)
xtasm = ftasm('tas')
ywt = area_weights(xtasm)
#
# This is a good way to compute the area fraction that the data is non-missing
#
ysum2 = averager(ywt, axis='xy', weight=['equal', 'equal'], action='sum')

#
# Verification of combine weights for accuracy in the presence of missing data
#
xavg_1 = averager(xtasm, axis = 'xy', weights = ywt)
xavg_2 = averager(xtasm, axis = 'xy', weights = ['generate', 'generate', 'generate'], combinewts=1)
assert MV2.allclose(xavg_1, xavg_2)

#
# Real world Averager Test #2
#
newf = cdms2.open(os.path.join(sys.prefix,'sample_data','clt.nc'))
u = newf('u')
u2 = averager(u, axis='1')
u3 = averager(u, axis='(plev)')
assert numpy.ma.allclose(u2, u3)


u4 = averager(u, axis='1x', weight=area_weights(u))
#
# SUM and AVERAGE should return the same answer when dimensions are averaged
# together or individually (in the same order)if the data has no missing values.
# Test this!
#
clt = newf('clt')

# First try the average
clt_ave = averager(clt, axis='txy')
clt1a = averager(clt, axis='t')
clt2a = averager(clt1a, axis='x')
clt3a = averager(clt2a, axis='y')
assert numpy.ma.allclose(clt_ave, clt3a)

# Now try the sum
clt_sum = averager(clt, axis='txy', action='sum')
clt1 = averager(clt, axis='t', action='sum')
clt2 = averager(clt1, axis='x', action='sum')
clt3 = averager(clt2, axis='y', action='sum')
assert numpy.ma.allclose(clt_sum, clt3)
