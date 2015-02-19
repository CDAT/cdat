import cdms2
import sys
import cdat_info
f=cdms2.open(cdat_info.get_prefix()+"/sample_data/clt.nc")
s=f("clt",latitude=(45,45,'cob'),longitude=(10,10,'cob'),squeeze=1)
print s.shape


import vcs
x=vcs.init()
l=x.createoneD()
l.smooth = 45
x.plot(s,l)

x.png("oneD")
raw_input("Press Enter")

