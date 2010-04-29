import sys
import cdms
import spanlib
import MV
#sys.path.insert(0,'../src/build/lib.linux-i686-2.4')


cdms.axis.latitude_aliases.append('Y')
cdms.axis.longitude_aliases.append('X')
cdms.axis.time_aliases.append('T')

f=cdms.open('../example/data2.cdf')

s2=f('ssta',latitude=(-10,10),longitude=(110,180))
s1=f('ssta',latitude=(-15,15),longitude=(210,250))


print 'Data  in:',s1.shape,s2.shape

res = spanlib.stackData(s1,s2)

print res[0].shape

SP=spanlib.SpAn(MV.array(res[0]),weights=MV.array(res[1]))
eof,pc,ev = SP.pca()


## for ax in res[3]:
##     ax[0]=eof.getAxis(0)

## res2 = spanlib.unStackData(eof,res[1],res[2],res[3])

res3 = steof,stpc,stev = SP.mssa(pca=True)

ffrec = SP.reconstruct()
res4 = spanlib.unStackData(ffrec,res[1],res[2],res[3])


print ffrec

import vcs



x=vcs.init()
x.plot(res4[1])
raw_input('ok?')
x.clear()
x.plot(res4[1][:,5,5])
x.plot(res4[0][:,5,5])
raw_input('ok?')
