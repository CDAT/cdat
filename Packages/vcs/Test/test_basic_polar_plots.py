import cdms2
import vcs
import os

x = vcs.init()
x.open()

b = vcs.createboxfill()

p = vcs.createprojection()
p.type = -3

b.projection = p

f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
s = f("clt", slice(0, 1), longitude=(0, 360), squeeze=1)

x.plot(s, b)
raw_input()

x.png("vcs_test_polar")
