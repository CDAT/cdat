# Adapted for numpy/ma/cdms2 by convertcdms.py
import sys
import MSU,cdms2


f=cdms2.open('../Data/ta.nc')
ta=f('ta',longitude=(0,360,'co'))
f.close()
f=cdms2.open('../Data/weights.nc')
w=f('weights')
f.close()
critw=0.5 # 50% of data required

msu=MSU.msu(ta,w,critw)

f=cdms2.open('../Data/tam2.nc')
goodtam2=f('tam2',longitude=(0,360,'co'))
f.close()
tam2=msu[...,0]
diff=tam2-goodtam2
import genutil
print genutil.minmax(diff)
if not cdms2.MV2.allclose(tam2,goodtam2):
    raise 'Error in MSU module, wrong result outputed for tam2, check if they are very different form 0, could be machine precision error'
else:
    print 'MSU seems ok, congratulations!'
