import vcs, numpy, cdms2, MV2, os, sys, vcs.testing.regression as regression

f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
T=f('clt')
v = regression.init()
v.plot(T,bg=1)
regression.run(v, 'first_png_blank.png')