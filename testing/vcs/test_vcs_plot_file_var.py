import vcs
import os
import sys
import cdms2
f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
V=f("clt")
x=vcs.init()
x.plot(V,bg=1)
