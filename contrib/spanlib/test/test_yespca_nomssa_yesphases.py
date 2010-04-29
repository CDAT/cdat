import sys
#sys.path.insert(0,'../src/build/lib.linux-i686-2.4')
import cdms
import spanlib
import vcs
import MV
import Numeric
import cdutil
import genutil

cdms.axis.latitude_aliases.append('Y')
cdms.axis.longitude_aliases.append('X')
cdms.axis.time_aliases.append('T')

f=cdms.open('../example/data2.cdf')

s=f('ssta',time=slice(0,120))
print s.shape

SP=spanlib.SpAn(s)

eof,pc,ev = SP.pca()

print 'Done PCA, doing phases w/o mssa'
print 'Reconstructing'

phases = SP.reconstruct(phases=True,nphases=16,firstphase=180,offset=0.7)

## phases = spanlib.phases(ffrec)

x=vcs.init()
for i in range(0,phases.shape[0],2):
    x.plot(phases[i])
    raw_input('map phase %i/%i ok?' % ( i+1 , phases.shape[0]))
    x.clear()
x.plot(phases[:,30,80])
raw_input('phases at center of bassin ok?')
x.clear()
