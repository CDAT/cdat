import cdms2
import os
import sys
import vcs.testing.regression as regression
import vcs
import numpy

data = sys.argv[2]
level = sys.argv[3]
levels = {'0': range(-5,36,5),
          '1': [-1000, -15, 35],
          '2': [-300, -15, 0, 15, 25],
          '3': range(190, 320, 10)}

x=regression.init(bg=1)
f=cdms2.open(data)
if (level == '3'):
    s=f("test")
else:
    s=f("sst")
iso=x.createisofill()
iso.levels=levels[level]
x.plot(s,iso)
regression.run(x, "test_vcs_isofill_level%s.png"%level)
