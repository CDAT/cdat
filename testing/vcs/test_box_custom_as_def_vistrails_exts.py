import vcs
import cdms2
import os
import sys

f=cdms2.open(os.path.join(vcs.prefix,"sample_data","clt.nc"))
s=f("clt",slice(0,1),squeeze=1)

x=vcs.init()

gm=x.createboxfill()
gm.boxfill_type="custom"
gm.levels=[1.e20,1.e20]
gm.ext_1="y"
gm.ext_2="y"

x.plot(s,gm)
raw_input("press enter")
