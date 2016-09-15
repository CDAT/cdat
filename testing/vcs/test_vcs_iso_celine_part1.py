import os,sys, MV2, numpy, vcs, cdms2, testing.regression as regression

src=sys.argv[1]
pth0 = os.path.dirname(__file__)
f = cdms2.open(os.path.join(pth0,"celine.nc"))
s = f("data")
x = regression.init()
x.setantialiasing(0)
x.setcolormap("classic")
x.scriptrun(os.path.join(pth0,"celine.json"))
i = x.getisofill("celine")
x.plot(s,i,bg=1)
fnm = "test_celine_iso.png"
regression.run(x, fnm)
