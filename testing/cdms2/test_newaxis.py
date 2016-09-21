from markError import clearError,markError,reportError
import MV2
import numpy
clearError()
print 'Test_newaxis: ... ',

try:
    b=MV2.ones((10,12))
    a=b[0]
    c=b+a[numpy.newaxis,:]
except:
    markError('dataset variable index slice: longitude array is wrong')

reportError()

