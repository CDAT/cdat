#!/usr/bin/env python

"""In this example we now retrieve NCEP and ECMWF for the year 1981, mask the land on their grid and apply an external data mask (the JONES variable). Finally the variables are once again put on the 10x10 grid where we apply a mask. Try not apply the mask on the final grid and note the difference (tip: to do so, simply change the definition of FG to: FG=cdutil.MaskedGridMaker() )
"""

import cdutil,os,sys,numpy
# First let's creates the mask (it is the same for NCEP and ECMWF since they are on the same grid).
refmsk = os.path.join(sys.prefix,'sample_data','sftlf_dnm.nc')
M=cdutil.WeightsMaker(refmsk, var='sftlf_dnm', values=[1.])
# Reference
ref = os.path.join(sys.prefix,'sample_data','tas_dnm-95a.xml')
Ref=cdutil.VariableConditioner(ref, weightsMaker=M)
Ref.var='tas'
Ref.id='D1'
Ref.cdmsKeywords={'time':('1979','1980','co')}
# Test
tstmsk = os.path.join(sys.prefix,'sample_data','sftlf_ccsr.nc')
M=cdutil.WeightsMaker(tstmsk, var='sftlf_ccsr', values=[1.])
tst = os.path.join(sys.prefix,'sample_data','tas_ccsr-95a.xml')
Tst=cdutil.VariableConditioner(tst, weightsMaker=M)
Tst.var='tas'
Tst.id='D2'
# External Variable (for the mask)
ext = ref
EV=cdutil.VariableConditioner(ext)
EV.var='tas'
EV.id='OUT'
# Final Grid
# We need a mask for the final grid
fgmask=ext
M2=cdutil.WeightsMaker(source=refmsk, var='sftlf_dnm', values=[["input",100.]])
FG=cdutil.WeightedGridMaker(weightsMaker=M2)
FG.longitude.n=36
FG.longitude.first=0.
FG.longitude.delta=10.
FG.latitude.n=18
FG.latitude.first=-85.
FG.latitude.delta=10.
# Now creates the compare object
c=cdutil.VariablesMatcher(Ref, Tst, weightedGridMaker=FG, externalVariableConditioner=EV)
# And gets it
(ref, reffrc), (test, tfrc) = c()
print 'Shapes:', test.shape, ref.shape
