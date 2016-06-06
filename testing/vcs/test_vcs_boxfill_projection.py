import os, sys, cdms2, vcs, testing.regression as regression

baselineName = sys.argv[1]
projection = sys.argv[2]


f = cdms2.open(vcs.sample_data + "/clt.nc")
a = f("clt")

x = regression.init()
p = x.getprojection(projection)
b = x.createboxfill()
b.projection = p
x.plot(a(latitude=(90,-90)), b, bg=1)

fileName = os.path.basename(baselineName)
fileName = os.path.splitext(fileName)[0]
fileName += '.png'

regression.run(x, fileName)
