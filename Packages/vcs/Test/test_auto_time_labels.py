# Adapted for numpy/ma/cdms2 by convertcdms.py
import sys
import cdms2
import vcs
import cdtime
import MV2
import support
import os

bg = support.bg

t0 = cdtime.comptime(1987, 8)
t1 = cdtime.comptime(1987, 12)
f = cdms2.open(os.path.join(vcs.sample_data, 'ta_ncep_87-6-88-4.nc'))

s = f('ta', latitude=slice(5, 6), level=slice(0, 1), squeeze=1)
# s.info()
x = vcs.init()
x.setantialiasing(0)


b = x.createboxfill('new2')
# b.list()

x.plot(s, b, bg=bg)
support.check_plot(x)
x.clear()
s = MV2.transpose(s)
x.plot(s, b, bg=bg)
support.check_plot(x)
