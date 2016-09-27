
import cdms2, os, sys, vcs, vcs.testing.regression as regression

cdmsfile = cdms2.open(os.path.join(vcs.sample_data,"clt.nc"))
data = cdmsfile('clt')
x = regression.init()
t=x.gettemplate('default')
x.plot(data, t, bg=True)

# This should force the image to update
x.setcolormap('blue2darkorange')
regression.run(x, "test_vcs_setcolormap.png")
