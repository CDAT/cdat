import os, sys, numpy, vcs, vcs.testing.regression as regression

x = regression.init()

data = numpy.array([1,2,3,4])
blon = numpy.array([-1,1,1,0,-1])
blat = numpy.array([0,0,1,2,1])
acell=numpy.array([blat,blon])
bcell = numpy.array([blat,blon+2.5])
ccell = numpy.array([blat+2.5,blon+2.5])
dcell = numpy.array([blat+2.5,blon])
mesh = numpy.array([acell,bcell,ccell,dcell])
m=x.createmeshfill()

x.plot(data,mesh,m,bg=1)
regression.run(x, "test_vcs_gen_meshfill.png")