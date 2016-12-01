import os, sys, MV2, numpy, vcs, cdms2, vcs.testing.regression as regression

src=sys.argv[1]
pth0 = os.path.dirname(__file__)
f = cdms2.open(os.path.join(pth0,"celine.nc"))
s = f("data")
x = regression.init()
x.setantialiasing(0)
x.scriptrun(os.path.join(pth0,"celine.json"))
x.setcolormap("classic")
i = x.getisofill("celine")
b = vcs.createboxfill()
b.levels=i.levels
b.fillareacolors=i.fillareacolors
b.boxfill_type="custom"
x.plot(s,b,bg=1)
fnm = os.path.split(__file__)[1][:-3] + ".png"
regression.run(x, fnm)
