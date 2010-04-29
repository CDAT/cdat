# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,sys,os

f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f('clt')

import genutil

print 'result:', genutil.statistics.variance(s,axis='t',weights=['equal'])
