import sys,cdutil
import vcs
import os
import cdms2

f=cdms2.open(os.path.join(sys.prefix,"sample_data","ta_ncep_87-6-88-4.nc"))
ta=f("ta",time=slice(0,1),squeeze=1)
ta=cdutil.averager(ta,axis="yx")
x=vcs.init()
print ta
print ta.getLevel()[:]
x.plot(ta)
raw_input("Press enter")
