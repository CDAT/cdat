import vcs
import cdms2
import sys
import os
import MV2

f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt",time=slice(0,1),squeeze=1)
s=MV2.masked_greater(s,67.)
x=vcs.init()
gm=x.createisofill()
gm=x.createboxfill()
x.plot(s,gm)
x.png(gm.g_name)
x.interact()



