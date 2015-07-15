import vcs
import os
import cdms2

x = vcs.init()
x.open()
x.scriptrun("Test/none_no_resize.json")

print x.listelements("oned")

y = x.getoneD("__yxvsx_152691238940216_yxvsx_")

f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
s = f("clt", latitude=(45, 45, "cob"), longitude=(23, 23, "cob"), squeeze=1)
# s+=100.
x.plot(s, y)
raw_input("ok")
