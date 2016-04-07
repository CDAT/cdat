import vcs
import cdms2
import os
import sys

f=cdms2.open(os.path.join(vcs.sample_data,"sampleCurveGrid4.nc"))
s=f("sample")
x=vcs.init()
m=x.createmeshfill()
m.datawc_y1=-85
m.datawc_y2=85
p=x.createprojection()
p.type="mercator"
m.projection=p
x.plot(s,m,bg=1)
x.png("meshfill_mercator")
raw_input("Press enter")
