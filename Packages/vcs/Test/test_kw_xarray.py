# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs
import cdms2 as cdms
import sys
import support
import os
bg = support.bg

f = cdms.open(os.path.join(vcs.sample_data, 'clt.nc'))
s = f('clt', time=slice(0, 2))

x = vcs.init()
x.plot(s, xarray=s.getAxis(-1)[:] * 2., bg=bg)
support.check_plot(x)
