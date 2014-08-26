import vcs,os
x=vcs.init()
x.open()
x.scriptrun("Test/jeff.json")

print x.listelements("oned")

y=x.getoneD("__yxvsx_152691238940216_yxvsx_")

import cdms2
import sys
f=cdms2.open(os.path.join(sys.prefix,"sample_data","clt.nc"))
s=f("clt",latitude=(45,45,"cob"),longitude=(23,23,"cob"),squeeze=1)
#s+=100.
x.plot(s,y)
raw_input("ok")


