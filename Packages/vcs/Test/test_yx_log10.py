# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2
import vcs
import cdutil
import support
import os
bg = support.bg
cdms2.setAutoBounds('on')
f = cdms2.open(os.path.join(vcs.sample_data, 'clt.nc'))
s = f('clt', time=slice(10, None), longitude=(10, 270))
s = cdutil.averager(s, axis='ty')
x = vcs.init()
yx = x.createyxvsx('new')
yx.xaxisconvert = 'log10'
x.plot(s, yx, bg=bg)
support.check_plot(x)
x.clear()
yx.xaxisconvert = 'linear'
x.plot(s, yx, bg=bg)
support.check_plot(x)
x.clear()
yx.xaxisconvert = 'log10'
x.plot(s, yx, bg=bg)
support.check_plot(x)
