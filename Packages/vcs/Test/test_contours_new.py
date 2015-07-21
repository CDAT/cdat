import vcs
import cdms2
import os
import MV2

f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
s = f("clt", time=slice(0, 1), squeeze=1)
s = MV2.masked_less(s, 65.)
x = vcs.init()
gm = x.createisofill()
gm.missing = 252
x.plot(s, gm)
x.png(gm.g_name)
x.interact()
