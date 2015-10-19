import vcs
import cdms2
import os

f = cdms2.open(os.path.join(vcs.sample_data, 'clt.nc'))
s = f("clt")
x = vcs.init()
bg = False
x.plot(s, bg=bg)
raw_input("Press enter")
x.clear()
raw_input("Press enter")
