import vcs, cdms2, numpy, os, sys
import vcs.testing.regression as regression

x = regression.init()
f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
lon1 = -180
u = f("clt")
v = f("clt")
u = u(longitude=(lon1,lon1+360.))
v = v(longitude=(lon1,lon1+360.))
V = x.createvector()
p = x.createprojection()
p.type = "robinson"
V.projection = p
x.plot(u,v,V, bg=1)
regression.run(x, "test_vcs_vectors_robinson_wrap.png")
