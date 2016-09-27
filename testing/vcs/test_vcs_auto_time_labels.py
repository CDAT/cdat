import vcs, cdms2, os, sys, vcs.testing.regression as regression

f = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
s = f("clt",longitude=slice(34,35),squeeze=1)
x = regression.init()
x.plot(s,bg=1)
regression.run(x, "test_vcs_auto_time_labels.png", sys.argv[1])