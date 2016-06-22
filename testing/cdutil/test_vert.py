#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py


import cdutil
import cdat_info
import numpy

import cdms2
import os
bg = 0

f = cdms2.open(os.path.join(cdat_info.get_sampledata_path(), 'vertical.nc'))
Ps = f('PS')
U = f('U')
B = f('hybm')
A = f('hyam')
Po = f('variable_2')
P = cdutil.reconstructPressureFromHybrid(Ps, A, B, Po)

U2 = cdutil.logLinearInterpolation(U, P)
U2b = cdutil.logLinearInterpolation(U, P, axis='0')
assert(numpy.ma.allclose(U2, U2b))
U2b = cdutil.logLinearInterpolation(U, P, axis='(lev)')
assert(numpy.ma.allclose(U2, U2b))
