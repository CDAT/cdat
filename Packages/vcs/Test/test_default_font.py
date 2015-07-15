# Adapted for numpy/ma/cdms2 by convertcdms.py
import sys
import vcs
import cdms2 as cdms
import support
import os
bg = support.bg
f = cdms.open(os.path.join(vcs.sample_data, 'clt.nc'))
s = f('clt')
x = vcs.init()
if len(sys.argv) > 1 and '--extended' not in sys.argv:
    font = sys.argv[1]
    try:
        ifont = int(font)
        x.setdefaultfont(ifont)
    except:
        x.addfont(font, "new")
        x.setdefaultfont("new")
else:
    x.setdefaultfont(7)
x.plot(s, bg=bg)
support.check_plot(x)
