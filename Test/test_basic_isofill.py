import vcs
import sys
import cdms2
f=cdms2.open(sys.prefix+'/sample_data/clt.nc')
s=f("clt")
x=vcs.init()
iso=x.createisofill()
x.plot(s,iso)
