import vcs, cdms2, numpy, os, sys
import vcs.testing.regression as regression

x = regression.init()
f = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
u = f("u")
v = f("v")
if (len(sys.argv) == 3 and sys.argv[2] == 'streamline'):
    V = x.createstreamline()
else:
    V = x.createvector()
p = x.createprojection()
p.type = "robinson"
V.projection = p
x.plot(u,v,V, bg=1)
fnm = os.path.split(sys.argv[1])[1][:-3] + "png"
regression.run(x, fnm)
