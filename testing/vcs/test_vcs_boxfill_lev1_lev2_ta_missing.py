import os, sys, cdms2, vcs, vcs.testing.regression as regression

x = regression.init()
f = cdms2.open(vcs.sample_data+"/ta_ncep_87-6-88-4.nc")
s = f("ta",slice(0,1),longitude=slice(34,35),squeeze=1)-273.15
s = cdms2.MV2.masked_less(s,-45.)
b = x.createboxfill()
b.level_1 = -40
b.level_2 = 40
x.plot(s, b, bg=1)
fnm = os.path.split(__file__)[1][:-3] + ".png"
regression.run(x, fnm)
