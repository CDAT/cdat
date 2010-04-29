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

s=f('ssta',time=slice(0,120),latitude=(-3,0),longitude=(128,135))
print s.shape
SP=spanlib.SpAn(s)

print 'Done PCA, doing phases with mssa'
steof,stpc,stev = SP.mssa(pca=False)

print 'Reconstructing no phases'

out = SP.reconstruct()

## phases = spanlib.phases(ffrec)

x=vcs.init()
for i in range(0,out.shape[0],out.shape[0]/10):
    x.plot(out[i])
    raw_input('map out %i/%i ok?' % ( i+1 , out.shape[0]))
    x.clear()
for i in range(0,out.shape[0],out.shape[0]/10):
    x.plot(s[i]-out[i])
    raw_input('noise map out %i/%i ok?' % ( i+1 , out.shape[0]))
    x.clear()
nj=out.shape[1]/3
ni=out.shape[2]/3
x.plot(out[:,nj,ni])
raw_input('time at center of bassin ok?')
x.clear()
