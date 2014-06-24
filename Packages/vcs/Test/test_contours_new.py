import vcs
import cdms2
import sys
import os

f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt",time=slice(0,1),squeeze=1)
x=vcs.init()
iso=x.createisofill()
x.plot(s,iso)
x.interact()



