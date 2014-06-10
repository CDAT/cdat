## Automatically adapted for numpy.oldnumeric Aug 01, 2007 by 

#!/usr/bin/env python

import numpy

errors = []

NTIME = 3
NLAT = 16
NLON = 32

x = numpy.ma.arange(float(2*NTIME*NLAT*NLON))
x.shape=(2,NTIME,NLAT,NLON)

def clearError():
    global errors
    errors = []

def markError(error,val=None):
    global errors
    if val is not None: error = error+': '+`val`
    errors.append(error)

def reportError():
    if errors==[]:
        print 'OK'
    else:
        print 'Failed: ',errors
