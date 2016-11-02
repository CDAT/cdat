import cdms2
import os
import sys
import vcs.testing.regression as regression
import vcs
import numpy

data = sys.argv[2]
x=regression.init(bg=1)
f=cdms2.open(data)
s=f["SST"]
iso=x.getisofill('a_robinson_isofill')
x.plot(s,iso)
regression.run(x, "test_vcs_isofill_masked_bounds.png")
