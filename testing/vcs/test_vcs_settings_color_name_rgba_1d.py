import vcs, numpy, os, sys, cdms2, vcs.testing.regression as regression

x = regression.init()

f=cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
data=f("clt")[:,5,8]
gm = x.create1d()
gm.linecolor="salmon"
gm.markercolor = [0,0,100]
x.plot(data,gm,bg=True)
regression.run(x, 'test_vcs_settings_color_name_rgba_1d.png')