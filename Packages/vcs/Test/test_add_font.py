# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs
import support
import os
bg = support.bg


x = vcs.init()

anb = x.getfontnumber("Arabic")
print 'font named arabic has number:', anb
nm = x.getfontname(1)
print 'font 1 is :', nm
tnb = x.getfont("Times")
print 'Times font has number', tnb
print 'now trying to switch fonts'
x.switchfonts(tnb, anb)
print 'font %i is now:' % tnb, x.getfontname(tnb)
print 'font %i is now:' % anb, x.getfontname(anb)
x.switchfonts(anb, tnb)
print 'font %i is now:' % tnb, x.getfontname(tnb)
print 'font %i is now:' % anb, x.getfontname(anb)
x.setdefaultfont(anb)

import cdms2 as cdms
f = cdms.open(os.path.join(vcs.sample_data, 'clt.nc'))
s = f('clt')
x.plot(s, bg=bg)
support.check_plot(x)
