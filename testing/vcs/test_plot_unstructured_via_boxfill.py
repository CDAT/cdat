import vcs
import os,sys
import cdms2


f=cdms2.open(os.path.join(vcs.prefix,"sample_data","sampleCurveGrid4.nc"))

s=f("sample")
x=vcs.init()

x.plot(s)
