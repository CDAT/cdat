import cdms2
import os
import sys

h = cdms2.open(sys.prefix + \
                   '/sample_data/so_Omon_ACCESS1-0_historical_r1i1p1_185001-185412_2timesteps.nc')
f = cdms2.open(sys.prefix + '/sample_data/clt.nc')

so = h('so')
clt = f('clt')
newSo = so.regrid(clt.getGrid())
try:
    os.remove('test.nc')
except:
    pass
k = cdms2.open('test.nc', 'w')
k.write(newSo)
