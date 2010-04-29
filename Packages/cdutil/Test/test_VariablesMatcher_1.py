#!/usr/bin/env python

import cdutil,os,sys
ref = os.path.join(cdutil.__path__[0],'..','..','..','..','sample_data','tas_dnm-95a.xml')
# Reference
Ref=cdutil.VariableConditioner(ref)
Ref.var='tas'
Ref.id='JONES'# optional
# Test
tst = os.path.join(cdutil.__path__[0],'..','..','..','..','sample_data','tas_ccsr-95a.xml')
Tst=cdutil.VariableConditioner(tst)
Tst.var='tas'
Tst.id='NCEP' #optional
# Final Grid
FG=cdutil.WeightedGridMaker()
FG.longitude.n=36
FG.longitude.first=0.
FG.longitude.delta=10.
FG.latitude.n=18
FG.latitude.first=-85.
FG.latitude.delta=10.
# Now creates the compare object.
c=cdutil.VariablesMatcher(Ref, Tst, weightedGridMaker=FG)
# And get it (3 different ways).
(ref, ref_frac), (test, test_frac) = c.get()
ref, test = c.get(returnTuple=0)
print ref,test
ref, test = c(returnTuple=0)
print ref,test
