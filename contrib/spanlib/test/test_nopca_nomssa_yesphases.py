import sys
#sys.path.insert(0,'../src/build/lib.linux-i686-2.4')
import cdms2 as cdms
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

print 'Reconstructing no phases'
out = SP.reconstruct(phases=True,nphases=16,end=2)

print 'Ok it should have errored!'
