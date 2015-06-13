# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,sys,os

f=cdms.open(os.path.join(vcs.sample_data,'clt.nc'))
s=f('clt')

import genutil

print 'result:', genutil.statistics.variance(s,axis='t',weights=['equal'])
