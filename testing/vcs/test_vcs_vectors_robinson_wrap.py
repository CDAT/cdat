import vcs, cdms2, numpy, os, sys
src = sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"..")
sys.path.append(pth)
import checkimage

x = vcs.init()
x.setantialiasing(0)
x.drawlogooff()
x.setbgoutputdimensions(1200, 1091, units="pixels")
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

fnm = "test_vcs_vectors_robinson_wrap.png"
x.png(fnm)
ret = checkimage.check_result_image(fnm, src, checkimage.defaultThreshold)
sys.exit(ret)
