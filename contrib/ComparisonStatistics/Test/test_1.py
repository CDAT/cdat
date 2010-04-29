#!/usr/bin/env python

import ComparisonStatistics
import cdutil
import os,sys

# Reference
ref = os.path.join(cdutil.__path__[0],'..','..','..','..','sample_data','tas_dnm-95a.xml')
Ref=cdutil.VariableConditioner(ref)
Ref.var='tas'
Ref.id='reference'

# Test
tst = os.path.join(cdutil.__path__[0],'..','..','..','..','sample_data','tas_ccsr-95a.xml')
Tst=cdutil.VariableConditioner(tst)
Tst.var='tas'
Tst.id='test'


# Final Grid
FG=cdutil.WeightedGridMaker()
FG.longitude.n=36
FG.longitude.first=0.
FG.longitude.delta=10.
FG.latitude.n=18
FG.latitude.first=-85.
FG.latitude.delta=10.

# Now the compall thing
c=ComparisonStatistics.ComparisonStatistics(Tst,Ref,weightedGridMaker=FG)
c.fracmin=.5
c.minyr=3
icall=19
# Let's force the indices to be the same
c.variableConditioner1.cdmsKeywords['time']=('1979','1982','co')
c.variableConditioner2.cdmsKeywords['time']=slice(0,36)

print "Before computing:"
print c.variableConditioner1
#print 'C printing:\n',c
## (test,tfr),(ref,reffrc)=c()
(test,tfr),(ref,reffrc) = c.compute()
print "Test:",test
# Retrieve the rank for th etime_domain 19 (monthly space time)
rank=c.rank(time_domain=19)


print 'Result for Rank:',rank
c.write('tmp.nc',comments='A simple example')
