import os, sys, cdms2, vcs, vcs.testing.regression as regression

f = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s = f("clt")
s3 = f("clt",longitude=(0,360))

x = regression.init()
x.plot(s,bg=1)
x.clear()
x.plot(s3,bg=1)
regression.run(x, "test_lon_axes_freak_out.png")