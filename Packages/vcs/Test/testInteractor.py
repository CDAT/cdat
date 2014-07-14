import vcs
import sys
import cdms2
import os

f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt",slice(0,1),squeeze=1)
x=vcs.init()
x.plot(s)
x.interact()

