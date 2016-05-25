import vcs, cdms2, sys, os, testing.regression as regression

f = cdms2.open(os.path.join(vcs.sample_data,'clt.nc'))
s = f("clt",slice(0,1),squeeze=1)
x = regression.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200,1091,units="pixels")
i = x.createisofill()
p = x.getprojection("polar")
i.projection=p
x.plot(s,i,bg=1)
regression.run(x, "test_polar_set_opt_param_polar.png")