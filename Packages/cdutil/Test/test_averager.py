# Adapted for numpy/ma/cdms2 by convertcdms.py
"""This is a set of tests for averager - which can be used to calculate area weighted averages."""
import cdms2, MV2, numpy, numpy.ma, cdtime, os, sys
from cdutil import area_weights, averager, AveragerError
cdms2.setAutoBounds('on')
f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','tas_ukmo_con.nc'))
x = f('tas')
ans = averager(x, axis='yt', weights = ['weighted','equal'], combinewts=1)
try:
    ans = averager(x, axis='yt', weights = ['weighted','equal'], combinewts=1)
except AveragerError:
    print 'Failure! 1'
#


try:
    ans2 = averager(x, axis='yx', weights = [numpy.ones(len(x.getLatitude()), numpy.float), 'generate'], combinewts=1)
except AveragerError:
    print 'Failure! 2'


b = numpy.array([1.,2.3, 3.0, 4.3])
b1 = numpy.ma.array(b)

try:
    b2,w = numpy.ma.average(b1, weights=None, returned=1)
    assert numpy.ma.allclose(b2, 2.65)
except AveragerError :
    print 'Failure! 3'


try:
    b2b,wb = averager(b1, weights=None, returned=1)
except AveragerError:
    print 'Failure! 4'
assert numpy.ma.allclose(b2b, 2.65)
assert numpy.ma.allclose(wb, w)


try:
    ba = averager(b)
except AveragerError:
    print 'failure! 5'
assert numpy.ma.allclose(ba, 2.65)


try:
    bs = averager(b, action='sum')
except AveragerError:
    print 'Failure! 6'
assert numpy.ma.allclose(bs, 10.6)



b = numpy.ones((3,4,5))
try:
    c = averager(b, weights=None, axis=[0,2], action='sum')
except AveragerError:
    print 'Failure! 7'
assert numpy.ma.allclose(c, [ 15., 15., 15., 15.,])
assert isinstance(c, numpy.ndarray)

try:
    c2 = averager(b, weights=numpy.ones(b.shape), axis=[0,2], action='sum')
    print 'Averager did not fail as it should have'
except AveragerError:
    pass


try:
    c2 = averager(b, weights=numpy.ones((3,5,4)), axis=[0,2], action='sum')
    print 'Averager did not fail as it should have'
except AveragerError:
    pass


try:
    d = averager(b, weights='equal', axis=2, action='sum')
except AveragerError:
    print 'Failure! 10'




d0MA = numpy.ma.average(b, weights=numpy.ma.array([1.,2.,3.,4.,5.]), axis=2)
try:
    d0 = averager(b, weights=numpy.ma.array([1.,2.,3.,4.,5.]), axis=2) 
except AveragerError:
    print 'Failure! 11'
assert numpy.ma.allclose(d0, d0MA)


try:
    d1 = averager(b, weights=numpy.ma.array([1.,2.,3.,4.,5.]), axis=2, action='sum')
except AveragerError:
    print 'Failure! 12'

try:
    d2 = averager(b, weights=[numpy.ma.array([1.,2.,3.]),'equal'], axis=[0,2], action='sum')
except  AveragerError:
    print 'Failure! 13'


b = numpy.array([1.,2.3, 3.0, 4.3])
try:
    bs = averager(b, action='sum')
except AveragerError:
    print 'Failure! 14'
assert numpy.ma.allclose(bs, 10.6)  


b = numpy.ones((3,4,5))
try:
    c = averager(b, weights=None, axis=[0,2], action='sum')
except AveragerError:
    print 'Failure! 15'
assert numpy.ma.allclose(c, [ 15., 15., 15., 15.,])
assert isinstance(c, numpy.ndarray)



b1 = numpy.ma.array(b)
try:
    c1 = averager(b1, weights=None, axis=[0,2])
except AveragerError:
    print 'Failure! 16'
assert isinstance(c1,numpy.ndarray)



try:
    averager(x, axis = 'xyt')
except AveragerError:
    print 'Failure! 17'

try:
    ans = averager(x)
except AveragerError:
    print 'Failure! 18'


try:
    averager(x, axis='2t')
except AveragerError:
    print 'Failure! 19'



try:
    averager(x, axis='t(lat)')
except AveragerError:
    print 'Failure! 20'



try:
    averager(x, axis='t(lev)')
    raise RuntimeError, "Test did not fail as it should."
except AveragerError:
    pass


try:
    averager(x, axis='t3')
    raise RuntimeError, "Test did not fail as it should."
except AveragerError:
    pass

try:
    averager(x, axis='t')
except AveragerError:
    print 'Failure! 23'


try:
    averager(x, axis='tx', weight=['generate', 'generate']) 
except AveragerError:
    print 'Failure! 24'

try:
    averager(x, axis='tx', weight=['equal', 'generate']) 
except AveragerError:
    print 'Failure! 25'



try:
    averager(x, axis='tx', weight='equal') 
    raise RuntimeError, "Test did not fail as it should."
except AveragerError:
    pass




try:
    a = numpy.array(range(10), numpy.float)
    averager(x, axis='tx', weight=['equal', a]) 
    raise RuntimeError, "Test did not fail as it should."
except AveragerError:
    pass




try:
    b=numpy.ma.array(a)
    averager(x, axis='tx', weight=['equal', b]) 
    raise RuntimeError, "Test did not fail as it should."
except AveragerError:
    pass



try:
    result = averager(x, axis='tx', weight=['equal', 'equal'])
except AveragerError:
    print 'Failure! 29'


try:
    result = averager(x, axis='2t', weight=['generate', 'equal'])
except AveragerError:
    print 'Failure! 30'


#**********************************************************************
#
# Create the area weights 
#
aw = area_weights(x)
#
#
#**********************************************************************


try:
    result = averager(x, axis='x', weight=aw)
except AveragerError:
    print 'Failure! 31'


try:
    result = averager(x, axis='xy', weight=aw) 
except AveragerError:
    print 'Failure! 32'


#
# Now I want the Longitude axis to be area weighted (including any missing data)
# but the latitude axis to be equally weighted
#
try:
    result, newwts = averager(x, axis='x', weight=aw, returned=1) 
    new_result = averager(result, axis='y', weight='equal')
except AveragerError:
    print 'Failure! 33'    



try:
    result = averager(x, axis='21', weight=aw) 
except AveragerError:
    print 'Failure! 34'

try:
    result = averager(x, axis=0) 
except AveragerError:
    print 'Failure! 35'


#******************************************************************************************
# Now a real world check!!
#******************************************************************************************

#
# The values in this file were calculated using old routines in CDAT2.4
#
fcheck = cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','tas_gavg_rnl_ecm.nc'))
start = cdtime.componenttime(1979, 1, 1)
end =  cdtime.componenttime(1979, 12, 1)
correct = fcheck('tas', time=(start, end))

#
# The source for the averages in the above file is the raw data in the file below.
# The difference in units (degrees C and degrees K) should be the obvious answer.
#
f = cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','tas_ecm_1979.nc'))

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
tasm_file = os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','tas_cru_1979.nc')

ftasm = cdms2.open(tasm_file)
xtasm = ftasm('tas')
ywt = area_weights(xtasm)
#
# This is a good way to compute the area fraction that the data is non-missing
#
try:
    ysum2 = averager(ywt, axis='xy', weight=['equal', 'equal'], action='sum')
except AveragerError:
    print 'Failure!'
    

#
# Verification of combine weights for accuracy in the presence of missing data
#
xavg_1 = averager(xtasm, axis = 'xy', weights = ywt)
xavg_2 = averager(xtasm, axis = 'xy', weights = ['generate', 'generate', 'generate'], combinewts=1)
assert MV2.allclose(xavg_1, xavg_2)

#
# Real world Averager Test #2
#
newf = cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
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
