import vcs, numpy, cdms2, MV2, os, sys, vcs.testing.regression as regression

x = regression.init()
f = cdms2.open(os.path.join(vcs.sample_data,"ta_ncep_87-6-88-4.nc"))
vr = "ta"
s=f(vr,slice(0,1),longitude=slice(90,91),squeeze=1,level=(0,10000))
x.plot(s,bg=1)
regression.run(x, 'test_vcs_flipNone.png')