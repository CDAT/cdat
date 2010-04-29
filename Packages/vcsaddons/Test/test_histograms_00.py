import vcsaddons,os
import vcs.test.support
bg=vcs.test.support.bg

h = vcsaddons.createhistogram()

import sys,cdms2
cdms2.setAutoBounds("on")
f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
s=f("clt")

h.datawc_x1=-.5
h.datawc_x2=119.5
h.datawc_y1=0.
h.datawc_y2=105

h.plot(s[:,6,8],bg=bg)
vcs.test.support.check_plot(h.x)
