import vcs
import cdms2
import sys
import os
import vcs.testing.regression as regression


pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)

x = regression.init(bg=1, geometry=(1620, 1080))

f = cdms2.open(vcs.sample_data + "/clt.nc")
s = f('clt')
iso = x.createisoline()
iso.level=[5, 50, 70, 95]
iso.linetypes = ['dot', 'dash', 'dash-dot', 'long-dash']
x.plot(s,iso,continents=0)
name = "test_vcs_line_patterns.png"
regression.run(x, name)
