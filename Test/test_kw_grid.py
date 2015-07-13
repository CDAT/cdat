# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs
import cdms2 as cdms
import sys
import support
import os
bg = support.bg

f = cdms.open(os.path.join(vcs.sample_data, 'clt.nc'))
s = f('clt')
g = s.getGrid()

d = s.filled() * 100.

x = vcs.init()
x.plot(d, grid=g, bg=bg)  # grid is just so we have the continnets
support.check_plot(x)
