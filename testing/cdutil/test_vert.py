#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py


import cdutil

import cdms2 as cdms,vcs,sys,os
bg = 0

f = cdms.open(os.path.join(sys.prefix,'sample_data','vertical.nc'))
Ps=f('PS')
U=f('U')
B=f('hybm')
A=f('hyam')
Po=f('variable_2')
P=cdutil.reconstructPressureFromHybrid(Ps,A,B,Po)

U2=cdutil.logLinearInterpolation(U,P)

#x=vcs.init()
#x.plot(U2,bg=bg)
#raw_input()
