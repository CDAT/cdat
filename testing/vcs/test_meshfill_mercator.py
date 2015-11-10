import vcs
import cdms2
import os
import sys

f=cdms2.open(os.path.join(vcs.sample_data,"sampleCurveGrid4.nc"))
s=f("sample")
x=vcs.init()
m=x.createmeshfill()
p=x.createprojection()
p.type="mercator"
m.projection=p
x.open()
x.geometry(800,600)
x.plot(s,m)
x.geometry(800,600)
raw_input("Press enter")
